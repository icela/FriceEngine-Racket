#lang racket
(require "engine.rkt")
(run-game (game (title "my first firce engine program")
                (bounds 200 200 600 500)
                (rectangle #:x 100 #:y 50 #:width 300 #:height 200 #:id "test")
                (oval #:x 100 #:y 100)
               ; (text #:content "hello world!!!")
                (text #:content "Fuck code forces!")
                (when-clicking-thunk #:object "test"
                                     #:thunk (lambda ()
                                       (tell "test" 'set 'x (+ (tell "test" 'get 'x)
                                                               10))))))