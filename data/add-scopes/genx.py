import sys
m=int(sys.argv[1]); mode=sys.argv[2]
print('#lang racket/base\n(require (for-syntax racket/base))\n(define-syntax (sp-ref stx) (syntax-case stx () [(_ id) #\'id]))\n(define x 1)')
for i in range(m):
    if mode=='fixed': rhs='(+ 1 (sp-ref x))'
    elif mode=='chain-nomacro-but-macro': rhs='(+ (sp-ref x) g%d)'%(i-1) if i else '0'
    elif mode=='expr': rhs=None
    if mode=='expr': print('(sp-ref x)')
    else: print('(define g%d %s)'%(i,rhs))
