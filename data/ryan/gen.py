#!/usr/bin/env python3
"""Generate teaching-language programs for Ryan's review question on racket/racket#5669:
deeply nested expressions (the teaching languages use local-expand to order syntax errors)
and one large flat program. Writes progs/<name>.rkt."""
import os

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'progs')
os.makedirs(OUT, exist_ok=True)

def write(name, lang, body):
    with open(f'{OUT}/{name}.rkt', 'w') as f:
        f.write(f'#lang {lang}\n{body}\n')

for n in [100, 300, 1000]:
    # nested arithmetic
    e = '1'
    for i in range(n):
        e = f'(+ {i} {e})'
    write(f'bsl-arith-{n}', 'htdp/bsl', f'(define r {e})\n(check-expect (number? r) #true)')
    # nested cond
    e = '0'
    for i in range(n):
        e = f'(cond [(= x {i}) {i}] [else {e}])'
    write(f'bsl-cond-{n}', 'htdp/bsl', f'(define (f x) {e})\n(check-expect (f 1) 1)')
    # nested local, each binding visible in the rest of the body
    body = f'v{n}'
    for i in range(n, 0, -1):
        body = f'(local [(define v{i} (+ v{i-1} 1))] {body})'
    write(f'isl-local-{n}', 'htdp/isl', f'(define v0 0)\n(define r {body})\n(check-expect r {n})')
    # nested lambda applications
    body = f'x{n}'
    for i in range(n, 0, -1):
        body = f'((lambda (x{i}) {body}) (+ x{i-1} 1))'
    write(f'islλ-lambda-{n}', 'htdp/isl+', f'(define x0 0)\n(define r {body})\n(check-expect r {n})')
    # nested let in ASL
    body = f'y{n}'
    for i in range(n, 0, -1):
        body = f'(let ([y{i} (+ y{i-1} 1)]) {body})'
    write(f'asl-let-{n}', 'htdp/asl', f'(define y0 0)\n(define r {body})\n(check-expect r {n})')

# a large flat program: many small functions with tests
defs = []
for i in range(2000):
    defs.append(f'(define (f{i} x) (cond [(< x {i}) (+ x {i})] [else (* x 2)]))\n'
                f'(check-expect (f{i} 0) {i if i > 0 else 0})')
write('bsl-large-2000', 'htdp/bsl', '\n'.join(defs))
defs = []
for i in range(2000):
    defs.append(f'(define (g{i} l) (local [(define (h y) (+ y {i}))] (map h l)))\n'
                f'(check-expect (g{i} (list 1)) (list {i + 1}))')
write('isl-large-2000', 'htdp/isl', '\n'.join(defs))
print(sorted(os.listdir(OUT)))
