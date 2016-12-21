#lang eopl
(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar-a
  '((program
     (expression)
     a-program)
    (expression
     (number)
     const-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     ("zero?" expression)
     zero?-exp)
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)))

(sllgen:make-define-datatypes scanner-spec-a grammar-a)
(define scan&parse-a (sllgen:make-string-parser scanner-spec-a grammar-a))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (eopl:error "expval extractor error" val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (eopl:error "expval extractor error" val)))))

(provide (all-defined-out))
