# FriceEngine on Lisp

Racket edition of frice engine is a little different from others(JVM/CLR).

# Demo
```
(require "engine.rkt")
(run-game (game (title "my first firceengine program")
                (bounds 200 200 600 500)

                (rectangle #:x 100 #:y 50 #:width 300 #:height 200 #:id "test")
                (oval #:x 100 #:y 100)
                (when-clicking-thunk #:object "test"
                                     #:thunk  (lambda ()
                                     (tell "test" 'set 'x (+ (tell "test" 'get  'x)                                                               
                                     10))))))
```
# Contributors

+ [Syntacticlosure](https://github.com/Syntacticlosure)



