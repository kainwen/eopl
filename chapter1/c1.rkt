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

;;1.23
(define list-index-helper
  (lambda (pred lst index)
    (if (null? lst)
        #f
        (if (pred (car lst))
            index
            (list-index-helper pred (cdr lst) (+ index 1)))))) 

(define list-index
  (lambda (pred lst)
    (list-index-helper pred lst 0)))

;;1.24
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))

;;1.25
(define exists?
     (lambda (pred lst)
       (if (null? lst)
           #f
           (if (pred (car lst))
               #t
               (exists? pred (cdr lst))))))

;;1.26
(define up
  (lambda (lst)
    (cond [(null? lst) '()]
          [(list? (car lst))
           (append (car lst)
                   (up (cdr lst)))]
          [else
           (cons (car lst)
                 (up (cdr lst)))])))

;;1.27
(define flatten
  (lambda (slist)
    (cond [(null? slist) '()]
          [(list? (car slist))
           (append (flatten (car slist))
                   (flatten (cdr slist)))]
          [else
           (cons (car slist)
                 (flatten (cdr slist)))])))

;;1.28
(define merge
  (lambda (loi1 loi2)
    (cond [(null? loi1) loi2]
          [(null? loi2) loi1]
          [(> (car loi1) (car loi2))
           (cons (car loi2)
                 (merge loi1 (cdr loi2)))]
          [else
           (cons (car loi1)
                 (merge (cdr loi1) loi2))])))

;;1.29
(define insert
  (lambda (i loi)
    (if (null? loi)
        (list i)
        (if (< i (car loi))
            (cons i loi)
            (cons (car loi)
                  (insert i (cdr loi)))))))

(define sort
  (lambda (loi)
    (if (null? loi)
        '()
        (insert (car loi)
                (sort (cdr loi))))))

;;1.30
(define insert/predict
  (lambda (pred i loi)
    (cond [(null? loi) (list i)]
          [(pred i (car loi))
           (cons i loi)]
          [else
           (cons (car loi)
                 (insert/predict pred i (cdr loi)))])))
  
(define sort/predict
  (lambda (pred loi)
    (if (null? loi)
        '()
        (insert/predict pred (car loi) (sort/predict pred (cdr loi))))))

;;1.31
(define leaf
  (lambda (n)
    n))

(define leaf?
  (lambda (tree)
    (integer? tree)))

(define interior-node
  (lambda (s lson rson)
    (list s lson rson)))

(define contents-of
  (lambda (tree)
    (if (leaf? tree)
        tree
        (car tree))))

(define lson
  (lambda (tree)
    (cadr tree)))

(define rson
  (lambda (tree)
    (caddr tree)))

;;1.32
(define double-tree
  (lambda (tree)
    (if (leaf? tree)
        (leaf (* 2 (contents-of tree)))
        (interior-node
         (contents-of tree)
         (double-tree (lson tree))
         (double-tree (rson tree))))))

;;1.33
(define make-leaves-with-red-depth-helper
  (lambda (tree num)
    (cond [(leaf? tree) (leaf num)]
          [(eqv? (contents-of tree) 'red)
           (interior-node
            'red
            (make-leaves-with-red-depth-helper (lson tree) (+ num 1))
            (make-leaves-with-red-depth-helper (rson tree) (+ num 1)))]
          [else
           (interior-node
            (contents-of tree)
            (make-leaves-with-red-depth-helper (lson tree) num)
            (make-leaves-with-red-depth-helper (rson tree) num))])))

(define make-leaves-with-red-depth
  (lambda (tree)
    (make-leaves-with-red-depth-helper tree 0)))

;;1.34
(define path-helper
  (lambda (n bst path-list)
    (cond [(null? bst) '()]
          [(= n (car bst)) path-list]
          [(< n (car bst))
           (path-helper n (cadr bst) (cons 'left path-list))]
          [else
           (path-helper n (caddr bst) (cons 'right path-list))])))

(define path
  (lambda (n bst)
    (reverse (path-helper n bst '()))))

;;1.35
(define number-leaves-helper
  (lambda (tree start_index)
    (cond
      [(leaf? tree) (list (leaf start_index) (+ 1 start_index))]
      [else
       (let ([l-result (number-leaves-helper (lson tree) start_index)])
         (let ([r-result (number-leaves-helper (rson tree) (cadr l-result))])
           (list (interior-node (contents-of tree)
                                (car l-result)
                                (car r-result))
                 (cadr r-result))))])))
             
(define number-leaves
  (lambda (tree)
    (car (number-leaves-helper tree 0))))

;;1.36
(define g
  (lambda (p l)
    (cons p
          (map (lambda (pr) (cons (+ 1 (car pr))
                                  (cdr pr)))
               l))))

(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

;;export all
(provide (all-defined-out))
