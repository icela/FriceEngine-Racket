#lang racket
(module frice-engine racket)
(provide game run-game title bounds showfps)
(require (rename-in pict [rectangle *rectangle]))
(require racket/gui)
(struct $game (title bounds showfps entitylist))
(define current-game #f)
(define-for-syntax syntax-keyword
  (lambda (e)
    (datum->syntax e (string->keyword (symbol->string (syntax->datum e))))))

(define-for-syntax keyword-trans
  (lambda (e)
    (syntax-case e ()
      [((p v) r ...) #`(#,(syntax-keyword #'p) [p v] #,@(keyword-trans #'(r ...)))]
      [() #`()])))

(define-syntax game
  (syntax-rules ()
    [(_ expr ...) (begin (set! current-game ($game "" '(0 0 0 0) #f '()))
                             expr ... current-game)]))

(define-syntax (define-game-updater stx)
  (syntax-case stx (bounds showfps entitylist : title)
    [(_ (name parameters ...) bounds : expr)
     #'(define (name parameters ...)
       (set! current-game ($game ($game-title current-game) 
                                  expr
                                 ($game-showfps current-game)
                                 ($game-entitylist current-game))))]
    [(_ (name parameters ...) showfps : expr)
     #'(define (name parameters ...)
       (set! current-game ($game ($game-title current-game)
                                 ($game-bounds current-game)
                                 expr
                                 ($game-entitylist current-game))))
     ]
    [(_ (name parameters ...) title : expr)
     #'(define (name parameters ...)
       (set! current-game ($game  expr 
                                 ($game-bounds current-game)
                                 ($game-showfps current-game)
                                 ($game-entitylist current-game))))]
    [(_ (name [parameter default-val] ...) entitylist : when-paint when-key when-mouse when-tick)
     #`(begin (define (name #,@(keyword-trans #'((parameter default-val) ...)))
         (set! current-game ($game ($game-title current-game)
                                   ($game-bounds current-game)
                                   ($game-showfps current-game)
                                     (cons (lambda args
                                               (match args
                                                 [`(set parameter ,val) (set! parameter val)] ...
                                                  [`(get parameter) parameter] ...
                                                  [`(get-paint) when-paint]
                                                  [`(get-key) when-key]
                                                  [`(get-tick) when-tick]
                                                  [else (error "Invalid Method")]))
                                           ($game-entitylist current-game)))
                                 ))
              (provide name))
              ]))

(define-game-updater (bounds x y width height) bounds : `(,x ,y ,width ,height))
(define-game-updater (showfps b) showfps : b)
(define-game-updater (title b) title : b)
(define-game-updater (rectangle [x 0]
                                [y 0]
                                [width 100]
                                [height 100]
                                [id "unbound"]
                                [border-width 1]
                                [border-color "black"]
                                [fill-color "nothing"]
                                ) entitylist : (lambda (dc)
                                                  (draw-pict (*rectangle width
                                                                         height
                                                                         #:border-width border-width
                                                                         #:border-color border-color) dc x y)
  )
                                                (lambda (key) (void))
                                                (lambda (x y) (void))
                                                (lambda () (void)))


(define-game-updater (oval [x 0]
                           [y 0]
                           [width 100]
                           [height 100]
                           [id "unbound"]
                           [border-width 1]
                           [border-color "black"]
                           [fill-color "nothing"]
                           ) entitylist : (lambda (dc)
                                                  (draw-pict (*rectangle width
                                                                         height
                                                                         #:border-width border-width
                                                                         #:border-color border-color) dc x y))
                                                (lambda (key) (void))
                                                (lambda (x y) (void))
                                                (lambda () (void)))
 

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
      (void)) 
    (define/override (on-char event) 
      (void)) 
    (super-new)))
  (define maincanvas (new my-canvas% [parent mainframe]))
  (define driver-loop (thread (lambda ()
                                (send maincanvas refresh-now
                                      (lambda (dc)
                                (for-each (lambda (x)
                                            ((x 'get-paint) dc )) elist))))))
  (send mainframe show #t))

(run-game (game (title "Helloworld!!")
                (rectangle #:width 300 #:height 100)
                (bounds 10 10 500 200)))





                                

             
