#lang lazy

(define leaf
  (lambda (n)
    (cons '() n)))

(define interior-node
  (lambda (n left right)
    (let ([left-tree-part (cdr left)]
          [right-tree-part (cdr right)])
      (letrec
       ([tree (cons '()
                    (cons n
                          (cons
                           (cons tree left-tree-part)
                           (cons tree right-tree-part))))])
       tree))))

(define move-left
  (lambda (node)
    (let ([tree-part (cdr node)])
      (cadr tree-part))))

(define move-right
  (lambda (node)
    (let ([tree-part (cdr node)])
      (cddr tree-part))))

(define move-up
  (lambda (node)
    (car node)))

(define contents
  (lambda (node)
    (let ([tree-part (cdr node)])
      (car tree-part))))

(define at-root?
  (lambda (node)
    (eqv? (move-up node) '())))


(define at-leaf?
  (lambda (node)
    (number? (cdr node))))

;;export all
(provide (all-defined-out))