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
   ;;1.23
   (check-equal?
    (list-index number? '(a 2 (1 3) b 7))
    1)
   (check-equal?
    (list-index symbol? '(a (b c) 17 foo))
    0)
   (check-equal?
    (list-index symbol? '(1 2 (a b) 3))
    #f)
   ;;1.24
   (check-equal?
    (every? number? '(a b c 3 e))
    #f)
   (check-equal?
    (every? number? '(1 2 3 4 5))
    #t)
   ;;1.25
   (check-equal?
    (exists? number? '(a b c 3 e))
    #t)
   (check-equal?
    (exists? number? '(a b c d e))
   #f)
   ;;1.26
   (check-equal?
    (up '((1 2) (3 4)))
    '(1 2 3 4))
   (check-equal?
    (up '((x (y)) z))
    '(x (y) z))
   ;;1.27
   (check-equal?
    (flatten '(a b c))
    '(a b c))
   (check-equal?
    (flatten '((a) () (b ()) () (c)))
    '(a b c))
   (check-equal?
    (flatten '((a b) c (((d)) e)))
    '(a b c d e))
   (check-equal?
    (flatten '(a b (() (c))))
    '(a b c))
   ;;1.28
   (check-equal?
    (merge '(1 4) '(1 2 8))
    '(1 1 2 4 8))
   (check-equal?
    (merge '(35 62 81 90 91) '(3 83 85 90))
    '(3 35 62 81 83 85 90 90 91))
   ;;1.29
   (check-equal?
    (sort '(8 2 5 2 3))
    '(2 2 3 5 8))
   ;;1.30
   (check-equal?
    (sort/predict < '(8 2 5 2 3))
    '(2 2 3 5 8))
   (check-equal?
    (sort/predict > '(8 2 5 2 3))
    '(8 5 3 2 2))
   ;;1.31
   (check-equal?
    (leaf 5)
    5)
   (check-equal?
    (leaf? (leaf 5))
    #t)
   (check-equal?
    (interior-node
     'a
     (leaf 5)
     (leaf 6))
    '(a 5 6))
   (check-equal?
    (lson
     (interior-node
      'a
      (leaf 5)
      (leaf 6)))
    5)
   (check-equal?
    (rson
     (interior-node
      'a
      (leaf 5)
      (leaf 6)))
    6)
   (check-equal?
    (contents-of
     (interior-node
      'a
      (leaf 5)
      (leaf 6)))
    'a)
   (check-equal?
    (contents-of
     (leaf 5))
    5)
   ;;1.32
   (check-equal?
    (double-tree (leaf 5))
    (leaf (* 2 5)))
   (check-equal?
    (double-tree
     (interior-node
      's
      (leaf 1)
      (leaf 2)))
    '(s 2 4))
   ;;1.33
   (check-equal?
    (make-leaves-with-red-depth
     (interior-node 'red
                    (interior-node 'bar
                                   (leaf 26)
                                   (leaf 12))
                    (interior-node 'red
                                   (leaf 11)
                                   (interior-node 'quux
                                                  (leaf 117)
                                                  (leaf 14)))))
    '(red (bar 1 1) (red 2 (quux 2 2))))
  ;;1.34
  (check-equal?
   (path 17 '(14 (7 () (12 () ()))
                 (26 (20 (17 () ())
                         ())
                     (31 () ()))))
   '(right left left))
  ;;1.35
   (check-equal?
    (number-leaves
     (interior-node 'foo
                    (interior-node 'bar
                                   (leaf 26)
                                   (leaf 12))
                    (interior-node 'baz
                                   (leaf 11)
                                   (interior-node 'quux
                                                  (leaf 117)
                                                  (leaf 14)))))
    '(foo
      (bar 0 1)
      (baz
       2
       (quux 3 4))))
   ;;1.36
   (check-equal?
    (number-elements '(a b c d))
    '((0 a) (1 b) (2 c) (3 d)))
  )
)

(require rackunit/text-ui)
(run-tests c1-tests)