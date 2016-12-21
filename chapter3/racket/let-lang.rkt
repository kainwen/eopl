#lang eopl
(require "front-end.rkt")
(require "env.rkt")

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp) (value-of exp (empty-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (zero?-exp
       (exp-test-zero)
       (let ([result-val (value-of exp-test-zero env)])
         (bool-val (= 0 (expval->num result-val)))))
      (if-exp
       (question answer alternate)
       (let ([question-val (value-of question env)])
         (if (expval->bool question-val)
             (value-of answer env)
             (value-of alternate env))))
      (diff-exp
       (exp1 exp2)
       (let ([result-exp1 (value-of exp1 env)]
             [result-exp2 (value-of exp2 env)])
         (num-val (- (expval->num result-exp1)
                     (expval->num result-exp2)))))
      (let-exp
       (id id-exp body)
       (let ([to-bind-val (value-of id-exp env)])
         (let ([new-env (extend-env id to-bind-val env)])
           (value-of body new-env)))))))

(provide (all-defined-out))