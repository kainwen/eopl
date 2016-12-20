#lang eopl
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define parse-polish-prefix-helper
  (lambda (prefix-list)
    (cond
      [(null? prefix-list) (cons '() '())]
      [(eqv? (car prefix-list) '-)
       (let ([result1 (parse-polish-prefix-helper (cdr prefix-list))])
         (let ([result2 (parse-polish-prefix-helper (cdr result1))])
           (cons
            (diff-exp
             (car result1)
             (car result2))
            (cdr result2))))]
      [(number? (car prefix-list))
       (cons
        (const-exp (car prefix-list))
        (cdr prefix-list))]
      [else (eopl:error "error!")])))

(define parse-polish-prefix
  (lambda (prefix-list)
    (car (parse-polish-prefix-helper prefix-list))))

(define unparse-polish-prefix
  (lambda (polish-prefix-abt)
    (cases prefix-exp polish-prefix-abt
      (const-exp (num) (list num))
      (diff-exp (operand1 operand2)
        `(- ,@(unparse-polish-prefix operand1) ,@(unparse-polish-prefix operand2))))))

;;export all
(provide (all-defined-out))
     