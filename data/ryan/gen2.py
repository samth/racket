#!/usr/bin/env python3
"""Generate programs that exercise make-interned-syntax-introducer, for Ryan's second
suggestion on racket/racket#5669. Interned scopes have negative ids, so they are the oldest
scopes in any set and sit at the end of the newest-first list.

- intro-D-K: racket/base; an identifier inside D nested `let`s gets K add/remove pairs
  from an interned (space-style) introducer at expansion time.
- rhm-block-N: Rhombus, N nested blocks, each defining a variable from the previous one;
  every Rhombus binding and reference goes through space introducers.
- rhm-fun-N: Rhombus, N nested functions."""
import os

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'progs')
os.makedirs(OUT, exist_ok=True)

MACRO = '''(require (for-syntax racket/base))
(define-syntax (space-ops stx)
  (syntax-case stx ()
    [(_ id k)
     (let ([intro (make-interned-syntax-introducer 'bench-space)])
       (for ([i (in-range (syntax-e #'k))])
         (intro (intro #'id 'add) 'remove))
       #'id)]))
'''
for d in [10, 100, 1000]:
    k = 20000
    body = f'(space-ops x{d} {k})'
    for i in range(d, 0, -1):
        body = f'(let ([x{i} {"x" + str(i - 1) if i > 1 else "0"}]) {body})'
    with open(f'{OUT}/intro-{d}-{k}.rkt', 'w') as f:
        f.write(f'#lang racket/base\n{MACRO}(define r {body})\n')

for n in [100, 300, 1000]:
    lines = ['#lang rhombus', 'def v0 = 0', 'def r:']
    ind = '  '
    for i in range(1, n + 1):
        lines.append(f'{ind}block:')
        ind += '  '
        lines.append(f'{ind}def v{i} = v{i-1} + 1')
    lines.append(f'{ind}v{n}')
    lines.append(f'check: r ~is {n}')
    with open(f'{OUT}/rhm-block-{n}.rkt', 'w') as f:
        f.write('\n'.join(lines) + '\n')
    lines = ['#lang rhombus', 'fun f0(x): x', 'def r:']
    ind = '  '
    for i in range(1, n + 1):
        lines.append(f'{ind}fun f{i}(x): f{i-1}(x) + 1')
        lines.append(f'{ind}block:')
        ind += '  '
    lines.append(f'{ind}f{n}(0)')
    lines.append(f'check: r ~is {n}')
    with open(f'{OUT}/rhm-fun-{n}.rkt', 'w') as f:
        f.write('\n'.join(lines) + '\n')
print(sorted(x for x in os.listdir(OUT) if x.startswith(('intro', 'rhm'))))
