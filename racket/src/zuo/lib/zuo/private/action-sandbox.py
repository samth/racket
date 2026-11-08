#!/usr/bin/env python3
"""Experimental Linux command-action sandbox. No dependency auto-learning.

Only explicit regular-file snapshots enter the filesystem. SDK manifests contain
{"version":1,"files":[absolute paths],"absent":[absolute paths]}. Absences are
checked on every lookup, including cache hits. Trace reports are diagnostics,
never declarations. This is a build correctness tool, not a hostile-code service.
"""
import argparse
import ast
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import stat
import struct
import subprocess
import sys
import tempfile

VERSION = 1
EPOCH = 946684800


def fail(message):
    raise ValueError(message)


def path(value):
    if not isinstance(value, str) or not value.startswith('/') or any(
            ord(c) < 32 or c in '<>"' for c in value):
        fail('expected an absolute, printable path: ' + repr(value))
    return os.path.normpath(value)


def inside(p, directory):
    return p == directory or p.startswith(directory.rstrip('/') + '/')


def manifest(filename):
    if filename is None:
        return [], [], [], []
    data = json.loads(Path(filename).read_text())
    if (not {'version', 'files', 'absent'}.issubset(data)
            or set(data) - {'version', 'files', 'absent', 'directories', 'metadata'}
            or data['version'] != VERSION):
        fail('unsupported SDK manifest')
    if not isinstance(data['files'], list) or not isinstance(data['absent'], list):
        fail('manifest files and absent must be lists')
    if not isinstance(data.get('directories', []), list):
        fail('manifest directories must be a list')
    metadata = data.get('metadata', [])
    if not isinstance(metadata, list):
        fail('metadata must be a list of file-presence paths')
    return (sorted(set(map(path, data['files']))), sorted(set(map(path, data['absent']))),
            sorted(set(map(path, data.get('directories', [])))), sorted(set(map(path, metadata))))


def digest(p):
    h = hashlib.sha256()
    with open(p, 'rb') as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b''):
            h.update(chunk)
    return h.hexdigest()


def interpreter(filename):
    """Kernel-loaded interpreters do not appear as userspace open syscalls."""
    with open(filename, 'rb') as f:
        header = f.read(256)
        if header.startswith(b'#!'):
            return path(os.fsdecode(header[2:].split(b'\n', 1)[0].split()[0]))
        if not header.startswith(b'\x7fELF'):
            return None
        if header[4] not in (1, 2) or header[5] not in (1, 2):
            fail('unsupported ELF format: ' + filename)
        endian = '<' if header[5] == 1 else '>'
        wide = header[4] == 2
        offset = struct.unpack_from(endian + ('Q' if wide else 'I'), header, 32 if wide else 28)[0]
        size, count = struct.unpack_from(endian + 'HH', header, 54 if wide else 42)
        if size < (56 if wide else 32) or count > 65535:
            fail('invalid ELF program headers: ' + filename)
        for i in range(count):
            f.seek(offset + i * size)
            program = f.read(size)
            if struct.unpack_from(endian + 'I', program)[0] == 3:  # PT_INTERP
                start = struct.unpack_from(endian + ('Q' if wide else 'I'), program, 8 if wide else 4)[0]
                length = struct.unpack_from(endian + ('Q' if wide else 'I'), program, 32 if wide else 16)[0]
                if length > 4096:
                    fail('oversized ELF interpreter')
                f.seek(start)
                return path(os.fsdecode(f.read(length).rstrip(b'\0')))
    return None


def plan(args):
    sdk, absent, directories, metadata = manifest(args.manifest)
    stdin_file = path(args.stdin_file) if args.stdin_file else None
    inputs = sorted(set(map(path, args.input)) | set(sdk) | ({stdin_file} if stdin_file else set()))
    outputs = sorted(set(map(path, args.output)))
    cwd = path(args.cwd)
    if cwd in ('/', '/tmp') or inside(cwd, '/dev'):
        fail('use a dedicated working directory, outside /dev and /tmp')
    if os.path.realpath(cwd) != cwd or not os.path.isdir(cwd):
        fail('working directory must exist and have no symlink ancestors')
    if not outputs or not all(inside(p, cwd) and p != cwd for p in outputs):
        fail('sandbox outputs must be below the working directory')
    if any(os.path.realpath(p) != p for p in outputs):
        fail('output paths must have no symlink components')
    if set(inputs) & set(outputs):
        fail('sandbox inputs and outputs overlap')
    if not args.command or path(args.command[0]) not in inputs:
        fail('the executable must be a declared input')
    records = []
    for p in inputs:
        if inside(p, '/tmp') or inside(p, '/dev') or inside(p, '/proc'):
            fail('inputs under /tmp, /dev and /proc are unsupported: ' + p)
        s = os.stat(p)
        if not stat.S_ISREG(s.st_mode):
            fail('only regular file inputs are supported: ' + p)
        if any(inside(out, p) or inside(p, out) for out in outputs):
            fail('input/output paths overlap: ' + p)
        records.append((p, digest(p), stat.S_IMODE(s.st_mode) & 0o777))
        if s.st_mode & 0o111:
            loader = interpreter(p)
            if loader and loader not in inputs:
                fail('declare the executable interpreter in the SDK manifest: ' + loader)
    for p in absent:
        if os.path.lexists(p):
            fail('declared absent path exists; update the SDK declaration: ' + p)
        if p in inputs or any(inside(q, p) for q in inputs + outputs + directories + metadata + [cwd]):
            fail('absence conflicts with a sandbox path: ' + p)
    for p in directories:
        if not os.path.isdir(p) or inside(p, '/proc') or inside(p, '/dev') or inside(p, '/tmp'):
            fail('unsupported declared directory: ' + p)
    for p in metadata:
        if (p in inputs or p in absent or p in directories
                or inside(p, '/proc') or inside(p, '/dev') or inside(p, '/tmp')
                or any(inside(out, p) or inside(p, out) for out in outputs)
                or any(inside(other, p) for other in inputs + metadata if other != p)):
            fail('conflicting file-presence declaration: ' + p)
        if not stat.S_ISREG(os.lstat(p).st_mode) or os.path.realpath(p) != p:
            fail('file-presence input must be a regular file without symlink ancestors: ' + p)
    signature = hashlib.sha256(json.dumps(
        [VERSION, records, absent, directories, metadata, stdin_file, cwd, outputs, args.command,
         args.unused, args.extra_outputs,
         digest(args.launcher)],
        separators=(',', ':')).encode()).hexdigest()
    return signature, records, absent, directories, metadata, cwd, outputs


QUOTED = re.compile(r'"(?:[^"\\]|\\.)*"')
CALL = re.compile(r'^(\d+)\s+([a-zA-Z0-9_]+)\((.*)\)\s+=\s+(.*)$')


def trace_calls(filename):
    pending = {}
    for raw in Path(filename).read_text().splitlines():
        if '<unfinished ...>' in raw:
            pid, fragment = raw.split(None, 1)
            if pid in pending:
                fail('overlapping unfinished trace records')
            pending[pid] = fragment.replace('<unfinished ...>', '')
            continue
        resumed = re.match(r'^(\d+)\s+<\.\.\. \w+ resumed>(.*)$', raw)
        if resumed:
            pid, rest = resumed.groups()
            if pid not in pending:
                fail('incomplete resumed trace record')
            raw = pid + ' ' + pending.pop(pid) + rest
        match = CALL.match(raw)
        if match:
            if any(match[3][q.end():].startswith('...') for q in QUOTED.finditer(match[3])):
                fail('truncated or unsupported trace record')
            yield int(match[1]), match[2], match[3], match[4]
        elif re.match(r'^\d+\s+\+\+\+ killed by .+ \+\+\+$', raw):
            yield int(raw.split()[0]), '__killed__', '', ''
        elif re.match(r'^\d+\s+(\+\+\+ exited with \d+ \+\+\+|--- .+ ---)$', raw):
            continue
        else:
            fail('unrecognized trace record: ' + raw[:120])
    if pending:
        fail('trace ended with unfinished system calls')


def audit(filename, inputs, absent, extra_directories, cwd, outputs, command,
          presence=(), stdin_file=None):
    declared = set(inputs)
    directories = {'/', '/tmp', '/dev', '/proc', cwd, *extra_directories}
    for p in list(inputs) + list(outputs) + list(presence) + [cwd]:
        directories.update(str(d) for d in Path(p).parents)
    used, metadata, created, problems = set(), set(), set(outputs), set()
    if stdin_file:
        used.add(stdin_file)  # delivered by the trusted launcher before exec
    processes = {}
    started = False
    exited = set()

    def resolve(value, state, argtext=None):
        if value.startswith('/'):
            return path(value)
        base = state['fs']['cwd']
        if argtext is not None:
            prefix = argtext.split('"', 1)[0]
            if not prefix.startswith('AT_FDCWD'):
                match = re.match(r'\d+<([^>]+)>', prefix)
                if not match:
                    fail('cannot resolve directory fd: ' + prefix)
                base = match[1]
        return path(os.path.join(base, value))

    def access(p, reading, success, writing=False):
        if writing:
            if p in declared or p in presence or not (inside(p, cwd) or inside(p, '/tmp')):
                problems.add('write outside scratch/outputs: ' + p)
            elif success:
                created.add(p)
            return
        if p in declared:
            (used if reading else metadata).add(p)
        elif p in presence:
            if reading:
                problems.add('content read of file-presence input: ' + p)
            else:
                metadata.add(p)
        elif p in created or inside(p, '/tmp') or p == '/dev/null':
            pass
        elif p in directories:
            if reading:
                # Listings contain only declared names or action-created scratch.
                pass
        elif p in absent and not success:
            pass
        else:
            problems.add(('undeclared read: ' if success else 'undeclared probe: ') + p)

    calls = list(trace_calls(filename))
    def child_pid(result):
        translated = re.search(r'/\* (\d+) in strace\'s PID NS \*/', result)
        return int(translated[1]) if translated else int(result.split()[0])

    forks = {child_pid(result): (pid, text)
             for pid, name, text, result in calls
             if name in ('clone', 'clone3', 'fork', 'vfork') and not result.startswith('-1 ')}

    def fork_state(parent, flags):
        child = dict(parent)
        child['fs'] = parent['fs'] if 'CLONE_FS' in flags else dict(parent['fs'])
        if 'CLONE_THREAD' not in flags:
            child.pop('namespace_pid', None)
        return child

    def descendant(pid, seen=None):
        if pid in processes:
            return True
        seen = set() if seen is None else seen
        if pid in seen or pid not in forks:
            return False
        seen.add(pid)
        parent, flags = forks[pid]
        if descendant(parent, seen):
            processes[pid] = fork_state(processes[parent], flags)
            return True
        return False

    for pid, name, text, result in calls:
        strings = [ast.literal_eval(m[0]) for m in QUOTED.finditer(text)]
        success = not result.startswith('-1 ')
        if pid not in processes and not (started and descendant(pid)):
            if name == 'execve' and success and strings and strings[0] == command[0]:
                if started:
                    fail('ambiguous action entry in trace')
                processes[pid] = {'fs': {'cwd': cwd}, 'exe': command[0]}
                started = True
            else:
                continue  # bubblewrap's trusted setup, outside the action
        state = processes[pid]
        if name in ('clone', 'clone3', 'fork', 'vfork'):
            if success:
                child = child_pid(result)
                if child not in processes:
                    processes[child] = fork_state(state, text)
            continue
        if name in ('exit', 'exit_group', '__killed__'):
            exited.add(pid)
            continue
        if name in ('wait4', 'waitid', 'kill', 'tkill', 'tgkill'):
            continue
        if name in ('socket', 'socketpair', 'connect', 'bind', 'sendto', 'sendmsg',
                    'recvfrom', 'recvmsg', 'listen', 'accept', 'accept4',
                    'getsockname', 'getpeername', 'setsockopt', 'getsockopt',
                    'shutdown', 'io_uring_setup', 'unshare', 'setns'):
            problems.add('unsupported network/namespace/asynchronous I/O syscall: ' + name)
            continue
        if name == 'getdents64':
            fail('directory enumeration is unsupported; declare an explicit file set')
        if name == 'fchdir':
            match = re.match(r'\d+<([^>]+)>', text)
            if not match:
                fail('cannot resolve fchdir')
            p = path(match[1])
            access(p, False, success)
            if success:
                state['fs']['cwd'] = p
            continue
        if name == 'getcwd':
            if success and (not strings or path(strings[0]) != state['fs']['cwd']):
                fail('getcwd disagrees with traced working directory')
            continue
        if not strings:
            fail('unsupported traced syscall: ' + name)
        at = name in ('openat', 'openat2', 'newfstatat', 'statx', 'faccessat',
                      'faccessat2', 'readlinkat', 'mkdirat', 'unlinkat',
                      'fchmodat', 'fchmodat2', 'utimensat', 'execveat')
        p = resolve(strings[0], state, text if at else None)
        if name == 'readlink' and p == '/proc/self' and success:
            if len(strings) != 2 or not re.fullmatch(r'[1-9][0-9]*', strings[1]):
                fail('unexpected private /proc/self target')
            state['namespace_pid'] = strings[1]
            continue
        proc_pid = '/proc/' + state.get('namespace_pid', 'unknown')
        if p == proc_pid and name in ('readlink', 'stat', 'lstat', 'newfstatat', 'statx'):
            continue  # metadata for this process in the private PID namespace
        proc_exe = p in ('/proc/self/exe', proc_pid + '/exe')
        if name == 'readlink' and proc_exe and success:
            if len(strings) != 2 or strings[1] != state['exe']:
                fail('unexpected /proc/self/exe target')
            metadata.add(state['exe'])
            continue
        if proc_exe:
            p = state['exe']  # private procfs alias for a declared executable
        if name in ('execve', 'execveat', 'open', 'openat', 'openat2'):
            writing = name.startswith('open') and bool(re.search(r'O_WRONLY|O_RDWR|O_CREAT|O_TRUNC', text))
            access(p, not writing and not re.search(r'O_PATH|O_DIRECTORY', text), success, writing)
            if name in ('execve', 'execveat') and success:
                if p not in declared:
                    problems.add('execution requires a declared tool: ' + p)
                state['exe'] = p
                seen = set()
                while p in declared and p not in seen:
                    seen.add(p)
                    loader = interpreter(p)
                    if not loader:
                        break
                    with open(p, 'rb') as f:
                        if f.read(2) == b'#!':
                            state['exe'] = loader
                    p = loader
                    access(p, True, True)
        elif name in ('stat', 'lstat', 'stat64', 'lstat64', 'newfstatat', 'statx',
                      'access', 'faccessat', 'faccessat2', 'readlink', 'readlinkat', 'chdir'):
            access(p, False, success)
            if name == 'chdir' and success:
                state['fs']['cwd'] = p
        elif name in ('mkdir', 'mkdirat', 'unlink', 'unlinkat', 'rmdir', 'chmod',
                      'fchmodat', 'fchmodat2', 'utime', 'utimes', 'utimensat', 'truncate'):
            access(p, False, success, True)
        elif name in ('rename', 'renameat', 'renameat2'):
            if len(strings) != 2:
                fail('cannot resolve rename')
            access(p, False, success, True)
            # Both directory descriptors are needed for *at variants.
            if name != 'rename':
                fail('renameat is not supported by this prototype')
            access(resolve(strings[1], state), False, success, True)
        else:
            fail('unsupported traced file syscall: ' + name)
    if not started or not exited or not set(processes).issubset(exited):
        fail('action trace is incomplete (entry/exit or descendant missing)')
    return {'version': VERSION, 'used': sorted(used), 'metadata_only': sorted(metadata - used),
            'unused': sorted(declared - used - metadata),
            'unused_metadata': sorted(set(presence) - metadata),
            'violations': sorted(problems)}


def run(args, expected):
    environment = sys.stdin.buffer.read()
    signature, records, absent, extra_directories, presence, cwd, outputs = plan(args)
    if signature != expected:
        fail('sandbox inputs changed before execution')
    with tempfile.TemporaryDirectory(prefix='zuo-sandbox-') as temp:
        root = Path(temp) / 'root'
        root.mkdir()
        def staged(p):
            return root / p.lstrip('/')
        directories = {cwd, '/tmp', '/dev', '/proc', *extra_directories}
        for p, sha, mode in records:
            target = staged(p)
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(p, target)
            target.chmod(mode)
            os.utime(target, (EPOCH, EPOCH))
            if digest(target) != sha:
                fail('input changed during snapshot: ' + p)
        for p in outputs:
            directories.add(str(Path(p).parent))
        for p in presence:
            target = staged(p)
            target.parent.mkdir(parents=True, exist_ok=True)
            target.touch(mode=0o444)
            os.utime(target, (EPOCH, EPOCH))
        for p in directories:
            staged(p).mkdir(parents=True, exist_ok=True)
        staged('/dev/null').touch()
        launcher = staged('/.zuo-launcher')
        shutil.copyfile(args.launcher, launcher)
        launcher.chmod(0o555)
        for directory, children, files in os.walk(root, topdown=False):
            os.chmod(directory, 0o755)
            os.utime(directory, (EPOCH, EPOCH))
        trace = Path(temp) / 'trace'
        command = [args.strace, '-f', '-qq', '-s', '65536', '-yy', '--decode-pids=pidns', '-e',
                   'trace=%file,%process,%network,getdents64,io_uring_setup', '-o', str(trace),
                   args.bubblewrap, '--unshare-all', '--new-session', '--die-with-parent',
                   '--cap-drop', 'ALL', '--hostname', 'zuo-action', '--ro-bind', str(root), '/',
                   '--bind', str(staged(cwd)), cwd, '--bind', str(staged('/tmp')), '/tmp',
                   '--dev-bind', '/dev/null', '/dev/null', '--proc', '/proc', '--chdir', cwd]
        for p, sha, mode in records:
            if inside(p, cwd):
                command.extend(['--ro-bind', str(staged(p)), p])
        for p in presence:
            if inside(p, cwd):
                command.extend(['--ro-bind', str(staged(p)), p])
        command.extend(['--', '/.zuo-launcher', args.stdin_file or '/dev/null', *args.command])
        result = subprocess.run(command, input=environment, env={})
        if args.trace_report:
            shutil.copyfile(trace, args.trace_report)
        report = audit(trace, [r[0] for r in records], absent, extra_directories, cwd, outputs,
                       args.command, presence, args.stdin_file)
        report['exit_status'] = result.returncode
        # Scratch intermediates are allowed, but retained files are reported as
        # output-contract candidates. Never silently add them to the output set.
        staged_inputs = {p for p, _, _ in records} | set(presence)
        report['undeclared_outputs'] = sorted('/' + str(p.relative_to(root))
            for p in staged(cwd).rglob('*')
            if (p.is_file() or p.is_symlink())
            and '/' + str(p.relative_to(root)) not in staged_inputs | set(outputs))
        if args.report:
            Path(args.report).write_text(json.dumps(report, indent=2) + '\n')
        for problem in report['violations']:
            print('sandbox: ' + problem, file=sys.stderr)
        for p in report['unused']:
            print('sandbox: unused declared input: ' + p, file=sys.stderr)
        for p in report['unused_metadata']:
            print('sandbox: unused file-presence input: ' + p, file=sys.stderr)
        if args.extra_outputs == 'error':
            for p in report['undeclared_outputs']:
                print('sandbox: undeclared retained output: ' + p, file=sys.stderr)
        if (result.returncode or report['violations']
                or ((report['unused'] or report['unused_metadata']) and args.unused == 'error')
                or (report['undeclared_outputs'] and args.extra_outputs == 'error')):
            fail('sandbox action rejected; no outputs published (exit ' + str(result.returncode) + ')')
        if plan(args)[0] != signature:
            fail('sandbox inputs changed during execution')
        # Validate the entire group before exposing any output to the host.
        for p in outputs:
            s = staged(p).lstat()
            if not stat.S_ISREG(s.st_mode):
                fail('output is not a regular file: ' + p)
            if any(part.is_symlink() for part in staged(p).parents if part != root.parent):
                fail('output has a symlink ancestor: ' + p)
        for p in outputs:
            dest = Path(p)
            dest.parent.mkdir(parents=True, exist_ok=True)
            fd, temporary = tempfile.mkstemp(prefix='.zuo-output-', dir=dest.parent)
            os.close(fd)
            try:
                shutil.copyfile(staged(p), temporary)
                os.chmod(temporary, stat.S_IMODE(staged(p).stat().st_mode) & 0o777)
                os.replace(temporary, p)
            finally:
                Path(temporary).unlink(missing_ok=True)


def main():
    if sys.platform != 'linux':
        fail('the command sandbox requires Linux')
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--manifest')
    parser.add_argument('--manifest-inputs', action='store_true')
    parser.add_argument('--plan', action='store_true')
    parser.add_argument('--expected-plan')
    parser.add_argument('--cwd')
    parser.add_argument('--input', action='append', default=[])
    parser.add_argument('--stdin-file', help='declared regular file delivered as action stdin')
    parser.add_argument('--output', action='append', default=[])
    parser.add_argument('--unused', choices=('error', 'report'), default='error')
    parser.add_argument('--extra-outputs', choices=('error', 'report'), default='report')
    parser.add_argument('--report')
    parser.add_argument('--trace-report')
    parser.add_argument('--bubblewrap', default='/usr/bin/bwrap')
    parser.add_argument('--strace', default='/usr/bin/strace')
    parser.add_argument('--launcher', required=True)
    parser.add_argument('command', nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if args.command[:1] == ['--']:
        args.command.pop(0)
    if args.manifest_inputs:
        print('\n'.join(manifest(args.manifest)[0]))
    elif args.plan:
        print(plan(args)[0])
    elif args.expected_plan:
        run(args, args.expected_plan)
    else:
        parser.error('expected --plan or --expected-plan')


if __name__ == '__main__':
    try:
        main()
    except (ValueError, OSError, KeyError, SyntaxError, struct.error) as e:
        print('sandbox: ' + str(e), file=sys.stderr)
        sys.exit(1)
