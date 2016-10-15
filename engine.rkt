#lang racket
(module frice-engine racket)
(provide game run-game title bounds showfps current-game current-entity-list tell reference)
(require (for-syntax "syntax-helper.rkt"))
(require (rename-in pict [rectangle *rectangle]))
(require racket/gui)
(require "generics.rkt")
(struct $game (title bounds showfps entitylist))
(define current-game (make-parameter '()))
(define current-entity-list (make-parameter '()))

(define-syntax game
  (syntax-rules ()
    [(_ expr ...) (parameterize ([current-game ($game "" '(0 0 0 0) #f '())])
                             expr ... (current-game))]))

(define-syntax (define-game-updater stx)
  (syntax-case stx (bounds showfps entitylist : title)
    [(_ (name parameters ...) bounds : expr)
     #'(define (name parameters ...)
       (current-game ($game ($game-title (current-game)) 
                                  expr
                                 ($game-showfps (current-game))
                                 ($game-entitylist (current-game)))))]
    [(_ (name parameters ...) showfps : expr)
     #'(define (name parameters ...)
       (current-game ($game ($game-title (current-game))
                                 ($game-bounds (current-game))
                                 expr
                                 ($game-entitylist (current-game)))))
     ]
    [(_ (name parameters ...) title : expr)
     #'(define (name parameters ...)
       (current-game ($game  expr 
                                 ($game-bounds (current-game))
                                 ($game-showfps (current-game))
                                 ($game-entitylist (current-game)))))]
    [(_ (name [parameter default-val] ...) entitylist : when-paint when-key when-mouse when-tick)
     #`(begin (define (name #,@(keyword-trans #'((parameter default-val) ...)))
         (current-game ($game ($game-title (current-game))
                                   ($game-bounds (current-game))
                                   ($game-showfps (current-game))
                                     (cons (lambda args
                                               (match args
                                                  [`(get-type) 'name]
                                                  [`(set parameter ,val) (set! parameter val)] ...
                                                  [`(get parameter) parameter] ...
                                                  [`(get-paint) when-paint]
                                                  [`(get-mouse) when-mouse]
                                                  [`(get-key) when-key]
                                                  [`(get-tick) when-tick]
                                                  [else (error "Invalid Method" args)]))
                                           ($game-entitylist (current-game))))
                                 ))
              (provide name))
              ]))

(define-syntax (define-shape stx)
  (syntax-case stx ()
    [(_ (name para ...) when-paint)
       #`(begin (define-class shape 'name)
              (define-game-updater (name [#,(datum->syntax stx 'x) 0]
                                         [#,(datum->syntax stx 'y) 0]
                                         [#,(datum->syntax stx 'width) 100]
                                         [#,(datum->syntax stx 'height) 100]
                                         [#,(datum->syntax stx 'id) "unbounded"]
                                         para ...) entitylist :
                when-paint
                (lambda (key) (void))
                (lambda (x y) (void))
                void))]))

(define-game-updater (bounds x y width height) bounds : `(,x ,y ,width ,height))
(define-game-updater (showfps b) showfps : b)
(define-game-updater (title b) title : b)
(define-shape  (rectangle [border-width 1]
                          [border-color "black"]
                          [fill-color "nothing"]
                          )  (lambda (dc)
                               (draw-pict (*rectangle width
                                                      height
                                                      #:border-width border-width
                                                      #:border-color border-color) dc x y)
                               )
)


(define-shape (oval [border-width 1]
                    [border-color "black"]
                    [fill-color "nothing"]
                    ) (lambda (dc)
                                     (draw-pict (ellipse width
                                                         height
                                                         #:border-width border-width
                                                         #:border-color border-color) dc x y)))

(define (reference id)
  (if (string=? id "unbound")
      (error "can't reference an unbound object.")
      (memf (lambda (x) (string=? id (x 'get 'id))) (current-entity-list))))

(define (tell id . msg)
  (apply (car (reference id)) msg))

(define-game-updater (when-clicking-thunk [id "unbound"]
                                          [object "unbound"]
                                          [thunk void])
  entitylist : (lambda (dc) (void))
               (lambda (key) (void))
               (lambda (ex ey) (call/cc (lambda (k) (for-each (lambda (e)
                                        (type-case e
                                          (shape
                                           (let ([ox (e 'get 'x)]
                                               [oy (e 'get 'y)]
                                               [ow (e 'get 'width)]
                                               [oh (e 'get 'height)])
                                           (when (in-rect? ex ey ox oy ow oh) (begin (thunk)
                                                                                   (k (void)))
                                               )))
                                          (else (void))))
                                          (current-entity-list)
                                                              )))) 
               (lambda () (void)))

(define in-rect?
  (lambda (ex ey ox oy ow oh)
    (and (<= ox ex (+ ox ow))
         (<= oy ey (+ oy oh)))))

                     

(define (run-game g)
  (define elist ($game-entitylist g))
  (define mainframe
   (match g
     [($game title `(,x ,y ,w ,h) showfps entitylist)
    (new frame% [label title][width w][height h][x x][y y])
    ]))
  (define my-canvas% 
    (class canvas% 
      (define/override (on-event event)
        (if (send event button-down? 'left)
        (let ([ax (send event get-x)]
              [ay (send event get-y)])
          (for-each (lambda (x)
                    ((x 'get-mouse) ax ay))
                      (current-entity-list)))
        (void)))
    (define/override (on-char event) 
      (void)) 
    (super-new)))
  (define maincanvas (new my-canvas% [parent mainframe]))
  (define driver-loop (lambda ()
                                (send maincanvas refresh-now
                                (lambda (dc)
                                (for-each (lambda (x)
                                            ((x 'get-paint) dc )) (current-entity-list)))
                                )(driver-loop)
                        ))
  (send mainframe show #t)
  (current-entity-list elist)
  (thread driver-loop)
  )



                                

             
