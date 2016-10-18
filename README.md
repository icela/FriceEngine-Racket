# FriceEngine on Lisp

Racket edition of frice engine is a little different from others(JVM/CLR).  
The latest version is 0.14.

# Demo (-FlappyBird)

```racket
#lang racket
(require "engine.rkt")
;;flappy bird

(game (bounds 600 480)
      (title "flappy bird")
      (oval #:id "bird" #:x 30 #:y 80 #:width 60 #:height 30 #:fill-color "orange"
            #:when-colliding (lambda ()
                               (tell "bird" 'set 'stop? #t)
                               (text #:content "You Lose" #:text-size 20
                                     #:x 250 #:y 200 #:color "red")
                               (tell "clock" 'set 'stop? #t)
                               )
            #:object-class "block"
            )
      (rectangle #:id "cilent" #:x 0 #:y 0 #:width 600 #:height 480)
      (when-left-clicking #:thunk (lambda ()
                                    (tell "bird" 'set 'velocity-y -0.16)
                                    (tell "bird" 'set 'accelerate-y 0.0005)))
      (when-left-clicking #:object-class "block"
                          #:thunk (lambda ()
                                    (display "hello")))
      
      (every #:interval 1800
             #:thunk (lambda ()
                       (rectangle #:x 600 #:y 0 #:height (random 50 260) #:width 20 #:velocity-x -0.1
                                  #:class "block" #:fill-color "green"
                                  )
                       (rectangle #:x 600 #:y 300 #:height (random 50 180) #:width 20 #:velocity-x -0.1
                                  #:class "block" #:fill-color "blue"
                                  )
                       )
             #:id "clock"
             )                              
      )
```

# API Reference
all objects share two properties : id & class  
In a certain game,two different objects can't have duplicate ids.
## Shapes : 
shared properties : x y width height when-colliding object object-class  
velocity-x velocity-y accelerate-x accelerate-y stop?  
rectangle : fill-color  
oval :fill-color  
text :content color text-size text-style  

## Events :
shared properties : object object-class thunk  
when-left-clicking  
when-right-clicking  
when-mouse-moving  
when-drapping  

## Timers :
every : interval  
stop?  
thunk  

## Object Managers :
tell `object-id` msg  
tellc `object-class` msg  
```
(tell "bird" 'set 'x 66)
```

# Contributors

+ [Syntacticlosure](https://github.com/Syntacticlosuredffddfs
