#lang racket/gui

(require "ship.rkt")
(require "obstacle.rkt")
(require "projectile.rkt")

(define FRAME_HEIGHT 400)
(define FRAME_WIDTH 1550)
(define LINE_COLOR (make-color 255 255 255))

(define START_X (/ (/ FRAME_HEIGHT 2) 2))
(define START_Y (* 2 (/ FRAME_HEIGHT 5)))
(define SPEED 4)
(define MAX_SPEED 7)
(define current-direction 'neutral)
(define previous-direction 'left)

(define is-ship-firing #f)
(define score 0)
(define bullet-counter 1)

(define health_points 5)
(define increment_speed 0.3)
(define LIMIT 4000)

(define time_elapsed 0)


(define main-frame
  (new frame%
       [label "space vehicle"]
       [width FRAME_WIDTH]
       [height FRAME_HEIGHT]
       )
  )

(define game-canvas%
  (class canvas%


    (define/private (custom-paint-callback canvas dc)

      ;draw background
      (send dc set-pen LINE_COLOR 8 'solid)
      (send dc draw-line 100 0 100 FRAME_HEIGHT)
      
      ;draw projectiles and ship
      (send proj1 draw dc)
      (send proj2 draw dc)
      (send proj3 draw dc)
      (send proj4 draw dc)
      (send proj5 draw dc)
      (send proj6 draw dc)
      (send proj7 draw dc)
      (send proj8 draw dc)
      (send proj9 draw dc)
      (send proj10 draw dc)
      (send spaceship draw dc)

      
      ;draw enemies
      (send pink_obstacle draw dc)
      (send green_obstacle draw dc)
      (send yellow_obstacle draw dc)

      ;draw UI
      (send dc set-text-foreground "green")
      (send dc draw-text "SCORE:" 200 0)
      (send dc draw-text (format "~v" score) 200 20)
      (send dc draw-text "HP" 150 0)
      (cond
        [(<= health_points 3) (send dc set-text-foreground "red")]
        )
      (send dc draw-text (format "~v" health_points) 150 20)

      ;manage start and end screens
      (when (equal? game-state 'start) 
        (send dc set-brush (make-color 0 0 0 0.5) 'solid)
        (send dc set-pen (make-color 0 0 0 0.5) 0 'solid)
        (send dc draw-rectangle 0 0 FRAME_WIDTH FRAME_HEIGHT)
        (send dc draw-text "Press escape to start!" (/ FRAME_WIDTH 2) (/ FRAME_HEIGHT 2))
        (send dc draw-text "Press space to fire, don't let them past you!" (-(/ FRAME_WIDTH 2) 80) (+(/ FRAME_HEIGHT 2) 20))
      )
      (when (equal? game-state 'ended) 
        (send dc set-brush (make-color 0 0 0 0.5) 'solid)
        (send dc set-pen (make-color 0 0 0 0.5) 0 'solid)
        (send dc draw-rectangle 0 0 FRAME_WIDTH FRAME_HEIGHT)
        (send dc draw-text "Game over :(" (/ FRAME_WIDTH 2) (/ FRAME_HEIGHT 2))
        (send dc draw-text "Final Score:" (-(/ FRAME_WIDTH 2) 50) (+(/ FRAME_HEIGHT 2) 20))
        (send dc draw-text (format "~v" score) (+(/ FRAME_WIDTH 2) 50) (+(/ FRAME_HEIGHT 2) 20))
      )

      )

    (super-new
     (paint-callback (lambda (canvas dc) (custom-paint-callback canvas dc)))
     )

    (send this set-canvas-background (make-color 0 0 0))
    
    (define/override (on-char event)
      ;handle player input
      (case (send event get-key-release-code)
        ['left (set! previous-direction current-direction) (set! current-direction 'left)]
        ['right (set! previous-direction current-direction) (set! current-direction 'right)]
        ['up (set! previous-direction current-direction) (set! current-direction 'up)]
        ['down (set! previous-direction current-direction) (set! current-direction 'down)]
        ['escape
          (case game-state
            ['start (set! game-state 'running) (send game-timer start 10)]
            ['ended (reset-game-state)
                    (set! game-state 'running)
                    (send game-timer start 10)])]
        ['#\space (set! is-ship-firing #t)]
          )
        )
      )
     )

(define game-state 'start)

(define game-timer
  (new timer%
       [notify-callback
        (lambda ()

          ;Handle player input, also move unfired projectiles alongside ship.
          (case current-direction
            ['left (unless (< (send spaceship get-x) 0)
                    (send spaceship set-x-position! (- (send spaceship get-x) SPEED))
                     (for ([projectile projectile-list])
                       (when (equal? (send projectile get-is-being-fired) #f)
                           (send projectile set-x-position! (+ (send spaceship get-x) 30))
                           )
                       )
                     )
                    ]
            ['right (unless (> (send spaceship get-x) 400)
                      (send spaceship set-x-position! (+ (send spaceship get-x) SPEED))
                      (for ([projectile projectile-list])
                       (when (equal? (send projectile get-is-being-fired) #f)
                           (send projectile set-x-position! (+ (send spaceship get-x) 30))
                           )
                       )
                      )
                    ]
            ['up  (unless (< (send spaceship get-y) 0)
                    (send spaceship set-y-position! (- (send spaceship get-y) SPEED))
                    (for ([projectile projectile-list])
                      (when (equal? (send projectile get-is-being-fired) #f)
                           (send projectile set-y-position! (+ (send spaceship get-y) 40))
                           )
                       )
                    )
                  ]
            ['down (unless (> (send spaceship get-bottom-y) FRAME_HEIGHT)
                     (send spaceship set-y-position! (+ (send spaceship get-y) SPEED))
                     (for ([projectile projectile-list])
                       (when (equal? (send projectile get-is-being-fired) #f)
                           (send projectile set-y-position! (+ (send spaceship get-y) 40))
                           )
                       )
                     )
                   ]
            )
          ;handle projectile "firing".
          (cond
            [(equal? is-ship-firing #t)
             (case bullet-counter
               [(1) (send proj1 fire)]
               [(2) (send proj2 fire)]
               [(3) (send proj3 fire)]
               [(4) (send proj4 fire)]
               [(5) (send proj5 fire)]
               [(6) (send proj6 fire)]
               [(7) (send proj7 fire)]
               [(8) (send proj8 fire)]
               [(9) (send proj9 fire)]
               [(10) (send proj10 fire)]
               )
             
             (if (equal? (+ bullet-counter 1) 11)
                 (set! bullet-counter 1)
                 (set! bullet-counter (+ bullet-counter 1))
                 )
             
             (set! is-ship-firing #f)
             ]
            )
          
          ;pushes vehicle to max speed in a direction upon double tapping a directional button.
          (cond
            [(equal? current-direction previous-direction)
             (case current-direction
               ['left (unless (< (send spaceship get-x) 0)
                        (send spaceship set-x-position! (- (send spaceship get-x) MAX_SPEED))
                        (for ([projectile projectile-list])
                          (when (equal? (send projectile get-is-being-fired) #f)
                            (send projectile set-x-position! (+ (send spaceship get-x) 30))
                            )
                          )
                        )
                      ]
               ['right (unless (> (send spaceship get-x) 400)
                         (send spaceship set-x-position! (+ (send spaceship get-x) MAX_SPEED))
                         (for ([projectile projectile-list])
                           (when (equal? (send projectile get-is-being-fired) #f)
                             (send projectile set-x-position! (+ (send spaceship get-x) 30))
                             )
                           )
                         )
                       ]
               ['up  (unless (< (send spaceship get-y) 0)
                       (send spaceship set-y-position! (- (send spaceship get-y) MAX_SPEED))
                       (for ([projectile projectile-list])
                         (when (equal? (send projectile get-is-being-fired) #f)
                           (send projectile set-y-position! (+ (send spaceship get-y) 40))
                           )
                         )
                       )
                     ]
               ['down (unless (> (send spaceship get-bottom-y) (- FRAME_HEIGHT 50))
                        (send spaceship set-y-position! (+ (send spaceship get-y) MAX_SPEED))
                        (for ([projectile projectile-list])
                          (when (equal? (send projectile get-is-being-fired) #f)
                            (send projectile set-y-position! (+ (send spaceship get-y) 40))
                            )
                          )
                        )
                      ]
               )
             ]
            )
          ;handle enemies advancing towards left and HP deductions.
          (for ([obstacle obstacle-list])
            (send obstacle move-obstacle)
            (when (< (get-field x-pos obstacle) 0)
              (set! health_points (- health_points 1))
              (cond
                [(or (< health_points 0) (= health_points 0)) (send game-timer stop)]
                )
              
              (send obstacle move-to-far)
              (cond
                [(or (< health_points 0) (= health_points 0))
                 (send game-timer stop)
                 (set! game-state 'ended)]
                )
              (case (get-field obstacle-color obstacle)
                ['pink (send obstacle set-y-position! (+ (random 320) 30))]
                ['yellow (send obstacle set-y-position! (+ (random 320) 30))]
                ['green (send obstacle set-y-position! (+ (random 320) 30))]
                )
              )
            )

          ; handle projectiles being "fired" and "trash collected" off screen. 
          (for ([projectile projectile-list])
            (when (equal? (send projectile get-is-being-fired) #t)
                (send projectile move-projectile)
                (when (> (get-field x-pos projectile) 1500)
                  (send projectile set-x-position! (+ (send spaceship get-x) 30))
                  (send projectile set-y-position! (+ (send spaceship get-y) 40))
                  (send projectile finish-firing)
                  )
                )
            )

          ;handle player collision with enemies.
          (for ([obstacle obstacle-list])
            (cond [(did-collide obstacle spaceship)
                   (set! health_points (- health_points 1))
                   (send obstacle move-to-far)
                   
                   (cond
                     [(or (< health_points 0) (= health_points 0))
                      (send game-timer stop)
                      (set! game-state 'ended)]
                     )
                   ]
                  
                  )
            )
          
          ;handle projectile collision with enemies and score addition
          (for ([projectile projectile-list])
            (for ([obstacle obstacle-list])
              (cond [(did-collide projectile obstacle)
                     (send obstacle move-to-far)
                     (send obstacle set-y-position! (+ (random 330) 30))
                     (send projectile finish-firing)
                     (send projectile set-x-position! (+ (send spaceship get-x) 30))
                     (send projectile set-y-position! (+ (send spaceship get-y) 40))
                     (define current_score score)
                     (set! score (+ current_score 1))
                     ]
                    )
              )
            )
          
          
          (set! time_elapsed (+ time_elapsed 1))

          (send main-canvas refresh-now)
          )
        ]
       )
  
  )


(define main-canvas
  (new game-canvas% [parent main-frame])
  )

(define spaceship (new ship%))

(define proj1 (new projectile%))
(send proj1 set-ship-position! START_X START_Y)
(send proj1 reset-position)

(define proj2 (new projectile%))
(send proj2 set-ship-position! START_X START_Y)
(send proj2 reset-position)

(define proj3 (new projectile%))
(send proj3 set-ship-position! START_X START_Y)
(send proj3 reset-position)

(define proj4 (new projectile%))
(send proj4 set-ship-position! START_X START_Y)
(send proj4 reset-position)

(define proj5 (new projectile%))
(send proj5 set-ship-position! START_X START_Y)
(send proj5 reset-position)

(define proj6 (new projectile%))
(send proj6 set-ship-position! START_X START_Y)
(send proj6 reset-position)

(define proj7 (new projectile%))
(send proj7 set-ship-position! START_X START_Y)
(send proj7 reset-position)

(define proj8 (new projectile%))
(send proj8 set-ship-position! START_X START_Y)
(send proj8 reset-position)

(define proj9 (new projectile%))
(send proj9 set-ship-position! START_X START_Y)
(send proj9 reset-position)

(define proj10 (new projectile%))
(send proj10 set-ship-position! START_X START_Y)
(send proj10 reset-position)

(define pink_obstacle (new obstacle% [obstacle-color 'pink]))
(send pink_obstacle set-x-position! FRAME_WIDTH)
(send pink_obstacle set-y-position! (+ 0 (+ (random 330) 30)))

(define green_obstacle (new obstacle% [obstacle-color 'green]))
(send green_obstacle set-x-position! FRAME_WIDTH)
(send green_obstacle set-y-position! (+ 0 (+ (random 330) 30)))

(define yellow_obstacle (new obstacle% [obstacle-color 'yellow]))
(send yellow_obstacle set-x-position! FRAME_WIDTH)
(send yellow_obstacle set-y-position! (+ 0 (+ (random 330) 30)))

(define projectile-list (list proj1 proj2 proj3 proj4 proj5 proj6 proj7 proj8 proj9 proj10))
(define obstacle-list (vector pink_obstacle yellow_obstacle green_obstacle))

(send spaceship set-x-position! START_X)
(send spaceship set-y-position! START_Y)

;handles collision between two objects, repurposed for both player-enemy collision and enemy-projectile collision.
(define (did-collide collision-object collidee)
  (define obstacle-left-x (send collision-object get-left-x))
  (define obstacle-right-x (send collision-object get-right-x))
  (define obstacle-bottom-y (send collision-object get-bottom-y))
  (define obstacle-top-y (send collision-object get-top-y))

  (define ship-left-x (send collidee get-left-x))
  (define ship-right-x (send collidee get-right-x))
  (define ship-bottom-y (send collidee get-bottom-y))
  (define ship-top-y (send collidee get-top-y))

  (and
   (< obstacle-left-x ship-right-x)
   (> obstacle-right-x ship-left-x)
   (< obstacle-top-y ship-bottom-y)
   (> obstacle-bottom-y ship-top-y)
   )
  )

;function that resets the game to its base state upon hitting an end screen.
(define (reset-game-state)
  (send spaceship reset-position)
  (send pink_obstacle move-to-far)
  (send green_obstacle move-to-far)
  (send yellow_obstacle move-to-far)
  (set! health_points 5)
  (set! score 0)
  (set! time_elapsed 0)
  (for [(projectile projectile-list)]
    (send projectile set-x-position! (+ (send spaceship get-x) 30))
    (send projectile set-y-position! (+ (send spaceship get-y) 40))
     )
  (for [(obstacle obstacle-list)]
    (send obstacle set-speed! 8)
    )
)

(send main-canvas refresh-now)

(send main-frame show #t)
