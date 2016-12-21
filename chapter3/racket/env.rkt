#lang eopl
(define empty-env
  (lambda ()
    '()))

(define is-empty?
  (lambda (env)
    (null? env)))

(define extend-env
  (lambda (var val env)
    (cons
     (cons var val)
     env)))

(define apply-env
  (lambda (env var)
    (cond
      [(is-empty? env)
       (eopl:error "can not find variable" var)]
      [(eqv? (caar env) var)
       (cdar env)]
      [else
       (apply-env (cdr env) var)])))

(provide (all-defined-out))