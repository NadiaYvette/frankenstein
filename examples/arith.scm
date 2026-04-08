; Baseline Scheme example: no call/cc, just a small computation.
; Expected result: 42

(define (sq x) (* x x))

(define main
  (+ (sq 5) (- 20 3)))   ; 25 + 17 = 42
