#lang eopl

(require rackunit
         "parse-polish-prefix.rkt")

(define parse-polish-prefix-tests
  (test-suite
   "unit test for parse-polish-prefix.rkt"
   (check-equal?
    (unparse-polish-prefix
     (parse-polish-prefix '(- - 3 2 - 4 - 12 7)))
    '(- - 3 2 - 4 - 12 7))
  )
)


(require rackunit/text-ui)
(run-tests parse-polish-prefix-tests)