#lang racket
(module frice-engine racket)
(provide game title bounds showfps tell reference tellc release-all)
(provide (all-from-out "events.rkt"))
(require "syntax-helper.rkt")
(require "data.rkt")
(require "events.rkt")
(require racket/stxparam)
(require (rename-in pict [rectangle *rectangle][text *text]))
(require racket/gui)

;;FriceEngine v0.14 : Update Contents
;;Use define-syntax-parameter
;;Remove generics.rkt
;;separate codes into different files


(define-shape  (rectangle [border-width 1]
                          [border-color "black"]
                          [fill-color "nothing"]
                          )  (lambda (dc)
                               (if (string=? fill-color "nothing")
                               (draw-pict (*rectangle width
                                                      height
                                                      #:border-width border-width
                                                      #:border-color border-color) dc x y)
                               (draw-pict (filled-rectangle width
                                                                    height
                                                                    #:border-width border-width
                                                                    #:border-color border-color
                                                                    #:color fill-color)
                                                    dc x y)
                               ))
)


(define-shape (oval [border-width 1]
                    [border-color "black"]
                    [fill-color "nothing"]
                    ) (lambda (dc)
                                     (if (string=? fill-color "nothing")
                                         (draw-pict (ellipse width
                                                         height
                                                         #:border-width border-width
                                                         #:border-color border-color) dc x y)
                                         (draw-pict (filled-ellipse width
                                                                    height
                                                                    #:border-width border-width
                                                                    #:border-color border-color
                                                                    #:color fill-color)
                                                    dc x y)
                                         )))

(define-shape (text [content ""][color "black"][text-size 13][text-style null])
  (lambda (dc)
    (let ([p (*text content text-style text-size)])
     (set! width (pict-width p))
     (set! height (pict-height p))
    (draw-pict (colorize p color
                         ) dc x y))))



                    

(define-syntax (game stx)
  (syntax-case stx ()
    [(_ exprs ...)
     #'(begin
         (define my-canvas% 
    (class canvas% 
      (define/override (on-event event)
        (let ([ax (send event get-x)]
              [ay (send event get-y)])
          (for-each (lambda (x)
            (when (send event button-down? 'left) ((x 'get-left-click) ax ay))
            (when (send event button-down? 'right) ((x 'get-right-down) ax ay))
            (when (send event moving?) ((x 'get-moving) ax ay))
            (when (send event dragging?) ((x 'get-dragging) ax ay (- ax cx) (- ay cy)))
            (when (send event button-up?) ((x 'get-release) ax ay))
                      )
                      (current-entity-list))
          (set! cx ax)
          (set! cy ay)
          )
        (void))
    (define/override (on-char event) 
      (void)) 
    (super-new)))
      (define cel (box '()))
      ($current-entity-list cel)
     (define mainframe
    (new frame% [label "No Title"][width 0][height 0][x 100][y 100]))
  (define cx 0)
  (define cy 0)
  (define maincanvas (new my-canvas% [parent mainframe]))
  (define maintimer (new timer% [notify-callback
                                 (lambda ()
                                   (let [(w  (send mainframe get-width))
                                        (h  (send mainframe get-height))]
                                   (filter (lambda (x)
                                               ((x 'get-tick))
                                               (if (memq (x 'get-type) (shapes))
                                                   (and (<= 0 (x 'get 'x) w)
                                                        (<= 0 (x 'get 'y) h))
                                                   #t))
                                                (current-entity-list))))]
                         [interval 20]))
  (define driver-loop (lambda ()
                                (send maincanvas refresh-now
                                (lambda (dc)
                                (for-each (lambda (x)
                                            ((x 'get-paint) dc )) (current-entity-list)))
                                )(driver-loop)
                        ))
  (send mainframe show #t)
  (syntax-parameterize ([title (syntax-rules ()
                                 [(_ n) (send mainframe set-label n)])]
                        [bounds (syntax-rules ()
                                 [(_ w h) (send mainframe resize w h)])])
                                  (begin exprs ...))
  (thread driver-loop)
  )]))



                                

             
