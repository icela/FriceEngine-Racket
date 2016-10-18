#lang racket
(require "data.rkt")
(require (for-syntax syntax/parse))
(provide define-game-updater define-shape)
(define-for-syntax syntax-keyword
  (lambda (e)
    (datum->syntax e (string->keyword (symbol->string (syntax->datum e))))))


(define-for-syntax keyword-trans
  (lambda (e)
    (syntax-case e ()
      [((p v) r ...) #`(#,(syntax-keyword #'p) [p v] #,@(keyword-trans #'(r ...)))]
      [() #`()])))


(define-syntax (define-game-updater stx)
  (syntax-case stx (:)
    [(_ (name [parameter default-val] ...) : (when-wtf expr) ...)
     (with-syntax ([id (datum->syntax stx 'id)]
                   [class (datum->syntax stx 'class)])
     #`(begin (define (name #,@(keyword-trans #'((parameter default-val) ...))
                            #:id [id "unbound"] #:class [class "unbound"])
                                     (current-entity-list
                                      (cons (lambda args
                                               (match args
                                                  [`(get id) id]
                                                  [`(set id ,v) (set! id v)]
                                                  [`(get class) class]
                                                  [`(set class ,v) (set! class v)]
                                                  [`(get-type) 'name]
                                                  [`(set parameter ,val) (set! parameter val)] ...
                                                  [`(get parameter) parameter] ...
                                                  [`(when-wtf) expr] ...
                                                  [else (lambda args (void))]))
                                            (current-entity-list))
                                           ))
              (provide name)))
              ]))

(define-syntax (define-shape stx)
  (syntax-case stx ()
    [(_ (name para ...) when-paint)
     (with-syntax ([y (datum->syntax stx 'y)]
                   [x (datum->syntax stx 'x)]
                   [vx (datum->syntax stx 'velocity-x)]
                   [vy (datum->syntax stx 'velocity-y)]
                   [ax (datum->syntax stx 'accelerate-x)]
                   [ay (datum->syntax stx 'accelerate-y)]
                   [when-colliding (datum->syntax stx 'when-colliding)]
                   [object (datum->syntax stx 'object)]
                   [object-class (datum->syntax stx 'object-class)]
                   [stop? (datum->syntax stx 'stop?)]
                   [w (datum->syntax stx 'width)]
                   [h (datum->syntax stx 'height)]
                   )
       #`(begin (shapes (cons 'name (shapes)))
           (define-game-updater (name [x 0]
                                    [y 0]
                                    [w 100]
                                    [h 100]
                                    [vx 0]
                                    [vy 0]
                                    [ax 0]
                                    [ay 0]
                                    [object "unbound"]
                                    [object-class "unbound"]
                                    [stop? #f]
                                    [when-colliding void]
                                         para ...) :
                (get-paint
                  when-paint)
             (get-tick
                 (lambda () (unless stop? (set! vx (+ vx (* 20 ax)))
                            (set! vy (+ vy (* 20 ay)))
                            (set! x (+ x (* 20 vx)))
                            (set! y (+ y (* 20 vy))))
                            (if (string=? object "unbound")
                                (if (string=? object-class "unbound")
                                    (void)
                                    (seq (cls := (ref-class object-class))
                                         (ox := (map (lambda (x) (apply x '(get x))) cls))
                                         (oy := (map (lambda (x) (apply x '(get y))) cls))
                                         (ow := (map (lambda (x) (apply x '(get width))) cls))
                                         (oh := (map (lambda (x) (apply x '(get height))) cls))
                                         (px1 := x) (px2 := (+ x w)) 
                                         (py1 := y) (py2 := (+ y h))
                                         (call/cc (lambda (k)(for ([ox ox]
                                                                   [oy oy]
                                                                   [ow ow]
                                                                   [oh oh])
                                                                                         (seq (ox1 := ox) (ox2 := (+ ox ow))
                                                                                              (oy1 := oy) (oy2 := (+ oy oh))
                                                                                         (when (or (and (<= ox1 px1 ox2)
                                                                                                      (<= oy1 py1 oy2))
                                                                                                 (and (<= ox1 px1 ox2)
                                                                                                      (<= oy1 py2 oy2))
                                                                                                 (and (<= ox1 px2 ox2)
                                                                                                      (<= oy1 py1 oy2))
                                                                                                 (and (<= ox1 px2 ox2)
                                                                                                      (<= oy1 py2 oy2)))
                                                                                             (when-colliding)
                                                                                             (k (void)))))))))
                                (call/cc (lambda (k) (seq (cls := (reference object))
                                         (ox := (apply cls '(get x)))
                                         (oy := (apply cls '(get y)))
                                         (ow := (apply cls '(get width)))
                                         (oh := (apply cls '(get height)))
                                     (seq (ox1 := ox) (ox2 := (+ ox ow))(oy1 := oy) (oy2 := (+ oy oh))
                                          (px1 := x) (px2 := (+ x w)) 
                                         (py1 := y) (py2 := (+ y h))
                                                                                         (when (or (and (<= ox1 px1 ox2)
                                                                                                      (<= oy1 py1 oy2))
                                                                                                 (and (<= ox1 px1 ox2)
                                                                                                      (<= oy1 py2 oy2))
                                                                                                 (and (<= ox1 px2 ox2)
                                                                                                      (<= oy1 py1 oy2))
                                                                                                 (and (<= ox1 px2 ox2)
                                                                                                      (<= oy1 py2 oy2)))
                                                                                             (when-colliding)
                                                                                             (k (void))))))
                                ))
             )))))]))

(define-syntax (seq stx)
  (define-syntax-class binder
    (pattern (var:id (~literal :=) val:expr)))
  (syntax-parse stx
    [(_ e:expr) #'e]
    [(_ (~or b:binder e:expr) rest ...) (if (attribute b)
                                         #'(let ([b.var b.val])
                                             (seq rest ...))
                                         #'(begin e (seq rest ...)))]
    ))

