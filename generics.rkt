#lang racket
(provide type-case define-class internal-relation)
;;;generic function system
(define internal-relation (make-parameter '()))
(define-syntax define-class
  (syntax-rules (quote)
    [(_ class-name sub ...)
     (begin (internal-relation (cons (list '(class-name) sub ...)
                              (internal-relation)))
     (new-relation! 'class-name (list sub ...)))
     ]))
;;a <= b

(define new-relation!
  (lambda (class-name sub)
    (internal-relation (map (lambda (x)
             (call/cc (lambda (k)
                      (map
                       (lambda (y)
                         (if (and (not (memq class-name (car x))) (memq y sub))
                             (k (cons (cons class-name (car x)) (cdr x)))
                                                     (void))) (car x))
                        x
                        )))
           (internal-relation)))))
(define subtype
  (lambda (a b lst)
    (if (eq? a b)
        #t
        (if (null? lst)
            #f
            (or (and (memq b (car (car lst)))
                     (memq a (cdr (car lst))))
                (subtype a b (cdr lst)))))))

(define-syntax type-case
  (syntax-rules (else)
    [(_ s (else st))
     st]
    [(_ s (type sts) r ...)
     (let ([res (if (symbol? s) s (s 'get-type))])
        (if (subtype res 'type (internal-relation))
            sts
            (type-case res r ...)))]
    [(_ s) (error "Nothing Matched")]))






