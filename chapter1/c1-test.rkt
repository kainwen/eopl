#lang racket
(require rackunit
         "c1.rkt")

(define c1-tests
  (test-suite
   "unit test for c1.rkt"
   ;;1.15
   (check-equal?
    (duple 0 '(blah))
    '())
   (check-equal?
    (duple 2 3)
    '(3 3))
   (check-equal?
    (duple 4 '(ha ha))
    '((ha ha) (ha ha) (ha ha) (ha ha)))
   ;;1.16
   (check-equal?
    (invert '((a 1) (a 2) (1 b) (2 b)))
    '((1 a) (2 a) (b 1) (b 2)))
   ;;1.17
   (check-equal?
    (down '(1 2 3))
    '((1) (2) (3)))
   (check-equal?
    (down '((a) (fine) (idea)))
    '(((a)) ((fine)) ((idea))))
   (check-equal?
    (down '(a (more (complicated)) object))
    '((a) ((more (complicated))) (object)))
   ;;1.18
   (check-equal?
    (swapper 'a 'd '(a b c d))
    '(d b c a))
   (check-equal?
    (swapper 'a 'd '(a d () c d))
    '(d a () c a))
   (check-equal?
    (swapper 'x 'y '((x) y (z (x))))
    '((y) x (z (y))))
   ;;1.19
   (check-equal?
    (list-set '(a b c d) 2 '(1 2))
    '(a b (1 2) d))
   (check-equal?
    (list-ref
     (list-set '(a b c d) 3 '(1 5 10))
     3)
    '(1 5 10))
   ;;1.20
   (check-equal?
    (count-occurrences 'x '((f x) y (((x z) x))))
    3)
   (check-equal?
    (count-occurrences 'x '((f x) y (((x z) () x))))
    3)
   (check-equal?
    (count-occurrences 'w '((f x) y (((x z) x))))
    0)
   ;;1.21
   (check-equal?
    (product '(a b c) '(x y))
    '((a x) (a y) (b x) (b y) (c x) (c y)))
   ;;1.22
   (check-equal?
    (filter-in number? '(a 2 (1 3) b 7))
    '(2 7))
   (check-equal?
    (filter-in symbol? '(a (b c) 17 foo))
    '(a foo))
  )
)

(require rackunit/text-ui)
(run-tests c1-tests)