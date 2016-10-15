#lang racket
(provide syntax-keyword keyword-trans)
(define syntax-keyword
  (lambda (e)
    (datum->syntax e (string->keyword (symbol->string (syntax->datum e))))))


(define keyword-trans
  (lambda (e)
    (syntax-case e ()
      [((p v) r ...) #`(#,(syntax-keyword #'p) [p v] #,@(keyword-trans #'(r ...)))]
      [() #`()])))


