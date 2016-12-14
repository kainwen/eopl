#lang racket
;;1.15
(define duple
  (lambda (n x)
    (if (= n 0)
        '()
        (cons x
              (duple (- n 1) x)))))

;;1.16
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (let ([first_pair (car lst)])
          (let ([a (car first_pair)]
                [b (cadr first_pair)])
            (cons (list b a)
                  (invert (cdr lst))))))))

;;1.17
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst))
              (down (cdr lst))))))

;;1.18
(define swapper
  (lambda (s1 s2 slist)
    (cond [(null? slist) '()]
          [(eqv? s1 (car slist))
           (cons s2 (swapper s1 s2 (cdr slist)))]
          [(eqv? s2 (car slist))
           (cons s1 (swapper s1 s2 (cdr slist)))]
          [(list? (car slist))
           (cons (swapper s1 s2 (car slist))
                 (swapper s1 s2 (cdr slist)))]
          [else
           (cons (car slist)
                 (swapper s1 s2 (cdr slist)))])))

;;1.19
(define list-set
  (lambda (lst n x)
    (if (= n 0)
        (cons x (cdr lst))
        (cons (car lst)
              (list-set (cdr lst) (- n 1) x)))))
;;1.20
(define count-occurrences
  (lambda (s slist)
    (cond ([null? slist] 0)
          ([eqv? s (car slist)]
           (+ 1
              (count-occurrences s (cdr slist))))
          ([list? (car slist)]
           (+ (count-occurrences s (car slist))
              (count-occurrences s (cdr slist))))
          (else
           (count-occurrences s (cdr slist))))))

;;1.21
(define product
  (lambda (sos1 sos2)
   (if (null? sos1)
       '()
       (let ([s1 (car sos1)])
         (append
          (map (lambda (s2) (list s1 s2)) sos2)
          (product (cdr sos1) sos2))))))

;;1.22
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst)
                  (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))


;;export all
(provide (all-defined-out))
