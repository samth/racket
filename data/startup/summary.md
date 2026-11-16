| command | metric | master min | master median | sets min | sets median | median change |
|---|---|---|---|---|---|---|
| `racket -n -e ''` | wall s | 0.050 | 0.050 | 0.050 | 0.050 | +0.00% |
| `racket -n -e ''` | instructions | 199,110,974 | 199,132,650 | 199,101,091 | 199,133,332 | +0.00% |
| `racket -n -e ''` | peak RSS MB | 64.1 | 64.1 | 64.1 | 64.1 | +0.05% |
| `racket -l racket/base -e ''` | wall s | 0.060 | 0.060 | 0.060 | 0.060 | +0.00% |
| `racket -l racket/base -e ''` | instructions | 359,816,345 | 359,984,486 | 359,319,652 | 359,471,676 | -0.14% |
| `racket -l racket/base -e ''` | peak RSS MB | 66.3 | 66.4 | 66.4 | 66.5 | +0.18% |
| `racket -l racket -e ''` | wall s | 0.140 | 0.140 | 0.140 | 0.140 | +0.00% |
| `racket -l racket -e ''` | instructions | 1,062,536,785 | 1,063,141,588 | 1,061,041,969 | 1,061,360,698 | -0.17% |
| `racket -l racket -e ''` | peak RSS MB | 102.9 | 103.4 | 103.0 | 103.3 | -0.08% |
| `racket -l typed/racket -e ''` | wall s | 0.250 | 0.260 | 0.250 | 0.250 | -3.85% |
| `racket -l typed/racket -e ''` | instructions | 2,118,059,581 | 2,118,699,298 | 2,088,176,769 | 2,088,645,912 | -1.42% |
| `racket -l typed/racket -e ''` | peak RSS MB | 139.9 | 140.2 | 140.1 | 140.3 | +0.02% |
| `racket -l racket/base -l drracket/private/link -e ''` | wall s | 1.230 | 1.250 | 1.220 | 1.230 | -1.60% |
| `racket -l racket/base -l drracket/private/link -e ''` | instructions | 10,277,829,217 | 10,284,002,691 | 10,209,632,793 | 10,217,838,550 | -0.64% |
| `racket -l racket/base -l drracket/private/link -e ''` | peak RSS MB | 442.4 | 443.2 | 439.1 | 441.5 | -0.39% |
