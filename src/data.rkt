#lang racket
(require racket/stxparam)
(provide (all-defined-out))
(define-syntax-parameter title (syntax-rules ()))
(define-syntax-parameter bounds (syntax-rules ()))
(define-syntax-parameter showfps (syntax-rules ()))
(define $current-entity-list (make-parameter #f))
(define shapes (make-parameter '()))
(define current-entity-list
  (case-lambda
    [() (unbox ($current-entity-list))]
    [(arg) (set-box! ($current-entity-list) arg)]
    ))

;;Object Management - ID CLASS
(define (ref-class cls)
  (let ([v  (filter (lambda (x) (string=? cls (x 'get 'class))
                  )
        (current-entity-list))])
       (if v v '())))

(define (tellc cls . msg)
  (map (lambda (x) (apply x msg)) (ref-class cls)))

  
(define (reference id)
  (if (string=? id "unbound")
      (error "can't reference an unbound object.")
      (car (filter (lambda (x) (string=? id (x 'get 'id))) (current-entity-list)))))

(define (tell id . msg)
  (apply (reference id) msg))

(define (release id)
  (current-entity-list
   (filter (lambda (x) (not (string=? id (x 'get 'id)))) (current-entity-list))))

(define (release-class class)
  (current-entity-list
   (filter (lambda (x) (not (string=? class (x 'get 'class)))) (current-entity-list))))

(define (release-all)
  (current-entity-list '()))

