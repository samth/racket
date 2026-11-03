#!/usr/bin/env python3
# Report the first bracket mismatch in a Scheme file (skips strings,
# ; comments, and #\ character literals) and the top-level form starts.
import sys
s = open(sys.argv[1]).read()
stack = []
i = 0
line = 1
pairs = {')': '(', ']': '['}
while i < len(s):
    c = s[i]
    if c == '\n':
        line += 1
    elif c == ';':
        while i < len(s) and s[i] != '\n':
            i += 1
        continue
    elif c == '"':
        i += 1
        while s[i] != '"':
            if s[i] == '\\':
                i += 1
            if s[i] == '\n':
                line += 1
            i += 1
    elif c == '#' and s[i + 1] == '\\':
        i += 3
        continue
    elif c in '([':
        stack.append((c, line))
    elif c in ')]':
        if not stack or stack[-1][0] != pairs[c]:
            print(f"mismatch {c} at line {line}; open {stack[-3:]}")
            sys.exit(1)
        stack.pop()
    i += 1
print("balanced" if not stack else f"unclosed {stack}")
