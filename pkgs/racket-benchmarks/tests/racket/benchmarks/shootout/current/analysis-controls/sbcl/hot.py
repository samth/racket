"""Map perf samples onto SBCL disassembly.

usage: python3 hot.py PROGRAM ARG
Records PROGRAM under perf (cpu_core/cycles/pp, P-core 6), then annotates
asm/PROGRAM.image.asm (disassembly printed by the same executable image)
with the share of samples on each instruction. Writes
asm/PROGRAM.annotated.asm and prints the hottest instructions.
"""
import collections, re, subprocess, sys

prog, arg = sys.argv[1], sys.argv[2]
data = "bin/%s.perf.data" % prog
subprocess.run(["taskset", "-c", "6", "perf", "record", "-q", "-e", "cpu_core/cycles/pp",
                "-c", "20011", "-o", data, "bin/" + prog, arg],
               stdout=subprocess.DEVNULL, check=True)
ips = collections.Counter()
total = 0
out = subprocess.run(["perf", "script", "-i", data, "-F", "ip,dso"], capture_output=True, text=True).stdout
for line in out.splitlines():
    parts = line.split()
    if not parts:
        continue
    total += 1
    ips[int(parts[0], 16)] += 1

lines = open("asm/%s.image.asm" % prog).read().splitlines()
annotated = []
covered = 0
hot = []
origin = None
prev_low = None
base = None
for l in lines:
    m = re.search(r"Origin: #x([0-9A-F]+)", l)
    if m:
        origin = int(m.group(1), 16)
        base = origin & ~0xFFF
        prev_low = origin & 0xFFF
        annotated.append("       " + l)
        continue
    m = re.match(r"; ([0-9A-F]{3}):\s+([0-9A-F]+)\s+(.*)", l)
    if m and origin is not None:
        low = int(m.group(1), 16)
        if low < prev_low:
            base += 0x1000
        prev_low = low
        addr = base + low
        n = sum(ips[a] for a in range(addr, addr + len(m.group(2)) // 2))
        covered += n
        pct = 100.0 * n / total if total else 0
        annotated.append("%6.2f %s" % (pct, l) if n else "       " + l)
        if n:
            hot.append((pct, l))
    else:
        annotated.append("       " + l)
open("asm/%s.annotated.asm" % prog, "w").write(
    ";; percent of all cpu_core/cycles/pp samples (%d samples) on each instruction\n" % total
    + "\n".join(annotated) + "\n")
print("%s: %d samples, %.1f%% inside the disassembled functions" % (prog, total, 100.0 * covered / total))
for pct, l in sorted(hot, reverse=True)[:12]:
    print("  %5.1f%%  %s" % (pct, l.strip()))
