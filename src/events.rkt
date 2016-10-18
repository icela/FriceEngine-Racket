#lang racket
(require "syntax-helper.rkt")
(require "data.rkt")
(define react-when
  (lambda (ex ey thunk pred)
  (call/cc
  (lambda (k) (for-each (lambda (e) (when (and
                                                    (pred e)
                                                    (memq (e 'get-type) (shapes)))
                                               (let ([ox (e 'get 'x)]
                                                     [oy (e 'get 'y)]
                                                     [ow (e 'get 'width)]
                                                     [oh (e 'get 'height)])
                                             (when (in-rect? ex ey ox oy ow oh) (begin (thunk)
                                                                                     (k (void)))
                                               )))
                                                                                )
                                                        (current-entity-list)
                                                        )))))

(define-game-updater (when-left-clicking [object "unbound"]
                                         [object-class "unbound"]
                                         [thunk void]) : (get-left-click (lambda (ex ey) (if (string=? object "unbound")
                                                                                             (if (string=? object-class "unbound")
                                                                                                 (thunk)
                                                                                                 (react-when ex ey thunk (lambda (e) (string=? (e 'get 'class) object-class))))
                                                                                                 (react-when ex ey thunk (lambda (e) (string=? (e 'get 'id) object)))))))


(define-game-updater (when-right-clicking [object "unbound"]
                                          [object-class "unbound"]
                                         [thunk void]) : (get-right-click (lambda (ex ey) (if (string=? object "unbound")
                                                                                             (if (string=? object-class "unbound")
                                                                                                 (thunk)
                                                                                                 (react-when ex ey thunk (lambda (e) (string=? (e 'get 'class) object-class)))
                                                                                                  )
                                                                                             (react-when ex ey thunk (lambda (e) (string=? (e 'get 'id) object)))))))

(define-game-updater (when-dragging [object "unbound"]
                                    [object-class "unbound"]
                                    [yet-dragging? #f]
                                    [dragging-out (lambda (x y) (void))]
                                    [thunk void]) : (get-dragging (lambda (ex ey mx my) (if (and (string=? object "unbound")
                                                                                            (string=? object-class "unbound"))
                                                                                            (thunk)
                                                                                            (call/cc (lambda (k) (for-each (lambda (e)
                                                     (when (and
                                                    (if (string=? object-class "unbound") (string=? object (e 'get 'id))
                                                        (string=? object-class (e 'get 'class)))
                                                    (memq (e 'get-type) (shapes)))
                                               (let* ([ox (e 'get 'x)]
                                                     [oy (e 'get 'y)]
                                                     [ow (e 'get 'width)]
                                                     [oh (e 'get 'height)]
                                                     [in? (in-rect? ex ey ox oy ow oh)]
                                                     )
                                             (when in? (set! yet-dragging? #t))
                                             (when (or yet-dragging? in?) (begin (thunk)
                                                                                  (e 'set 'x (+ mx (e 'get 'x)))
                                                                                  (e 'set 'y (+ my (e 'get 'y)))
                                                                                     (k (void)))
                                               )))
                                                                                )
                                                        (current-entity-list))
                                                        )))))
  (get-release (lambda (x y) (dragging-out x y)(set! yet-dragging? #f)))
  )


(define-game-updater (when-mouse-moving [object "unbound"]
                                        [object-class "unbound"]
                                        [thunk void]) : (get-moving (lambda (ex ey) (if (string=? object "unbound")
                                                                                             (if (string=? object-class "unbound")
                                                                                                 (thunk)
                                                                                                 ((react-when ex ey thunk (lambda (e) (string=? (e 'get 'class) object-class)))
                                                                                                  (lambda (x) x)))
                                                                                             (call/cc (react-when ex ey thunk (lambda (e) (string=? (e 'get 'id) object))))))))


(define-game-updater (every [interval 20]
                            [process 0]
                            [thunk void]
                            [stop? #f]) : (get-tick (lambda ()
                           (set! process (+ process 20))
                           (when (>= process interval) (begin (set! process 0) (unless stop? (thunk)))))))



(define in-rect?
  (lambda (ex ey ox oy ow oh)
    (and (<= ox ex (+ ox ow))
         (<= oy ey (+ oy oh)))))