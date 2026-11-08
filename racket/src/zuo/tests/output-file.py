#!/usr/bin/env python3
"""Regression for per-output dependency identity and complete producer groups.
Usage: python3 tests/output-file.py /absolute/path/to/zuo
"""
from pathlib import Path
import subprocess
import os
import sys
import tempfile

exe = Path(sys.argv[1]).resolve()
driver = Path(__file__).with_name('output-file-driver.zuo')
with tempfile.TemporaryDirectory(prefix='zuo-output-file-') as temp:
    root = Path(temp)
    def run():
        p = subprocess.run([str(exe), str(driver), '-j', '4'], cwd=root,
                           capture_output=True, text=True)
        assert p.returncode == 0, p.stdout + p.stderr
    def count(): return (root / 'runs').read_text().count('run\n')
    # Some supported Zuo runtimes expose only whole-second mtimes. Make
    # source mutations unambiguous without sleeping or testing that cache.
    timestamp = [root.stat().st_mtime_ns]
    def write_input(text):
        timestamp[0] += 2_000_000_000
        (root / 'input').write_text(text)
        os.utime(root / 'input', ns=(timestamp[0], timestamp[0]))
    write_input('one\ntwo\n')
    run(); assert count() == 1
    run(); assert count() == 1
    # A changed sibling triggers the producer but not this consumer.
    write_input('one\nthree\n')
    run(); assert count() == 1 and (root / 'b').read_text() == 'three'
    # A missing co-output is repaired even when the requested output exists.
    (root / 'b').unlink()
    run(); assert count() == 1 and (root / 'b').exists()
    (root / 'a').unlink()
    run(); assert count() == 1 and (root / 'a').read_text() == 'one'
    write_input('four\nthree\n')
    run(); assert count() == 2 and (root / 'result').read_text() == 'four'
print('Per-output dependency regression passed')
