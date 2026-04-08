; Scheme call/cc escape demo.
;
; Without call/cc, (+ 10 (bomb 7)) would compute bomb then add 10.
; Here, bomb calls its received continuation k with 100, which escapes
; the surrounding (+ 10 ...) and returns 100 directly from call/cc.
;
; Expected result: 100  (NOT 110 — the +10 is bypassed)

(define main
  (call/cc
    (lambda (k)
      (+ 10 (k 100)))))
