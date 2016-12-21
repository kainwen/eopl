#lang eopl
(require "front-end.rkt")
(require rackunit
         "let-lang.rkt")

(define let-lang-tests
  (test-suite
   "unit test for let-lang.rkt"
   (check-equal?
    (expval->num (value-of-program (scan&parse-a "let a = 1 in -(a, 3)")))
    -2
   )
  )
)


(require rackunit/text-ui)
(run-tests let-lang-tests)