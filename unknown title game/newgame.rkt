#lang racket/gui
;(require "car.rkt")
;(require "square.rkt")

(require "ship.rkt")
(require "obstacle.rkt")

(define FRAME_HEIGHT 400)
(define FRAME_WIDTH 1550)
(define LINE_COLOR (make-color 141 0 0))
(define START_X (/ (/ FRAME_HEIGHT 2) 2))
(define START_Y (* 2 (/ FRAME_HEIGHT 5)))
(define SPEED 4)
(define MAX_SPEED 7)
(define current-direction 'neutral)
(define previous-direction 'left)
(define health_points 100)


(define main-frame
  (new frame%
       [label "space vehicle"]
       [width FRAME_WIDTH]
       [height FRAME_HEIGHT]
       )
  )

(define game-canvas%
  (class canvas%
#|
     (define/override (on-char event)
      (case (send event get-key-release-code)
        ['left (send spaceship change-direction)]
        ['right (send spaceship change-direction)]
        )
       (send this refresh-now)
       )
    |#

    (define/private (custom-paint-callback canvas dc)
      ;draw background

      
      (send dc set-pen LINE_COLOR 8 'solid)
      (send dc draw-line 100 0 100 FRAME_HEIGHT)
      ;draw things
      (send spaceship draw dc)
      
      ;draw obstacles
      (send red_obstacle draw dc)
      (send orange_obstacle draw dc)
      (send yellow_obstacle draw dc)
      (send dc set-text-foreground "green")
      (send dc draw-text "hp" 30 0)
      (cond
        [(<= health_points 30) (send dc set-text-foreground "red")]
        )
      (send dc draw-text (format "~v" health_points) 30 20)
      (when (equal? game-state 'start) 
        (send dc set-brush (make-color 0 0 0 0.5) 'solid)
        (send dc set-pen (make-color 0 0 0 0.5) 0 'solid)
        (send dc draw-rectangle 0 0 FRAME_WIDTH FRAME_HEIGHT)
        (send dc draw-text "Press space to start!" (/ FRAME_WIDTH 2) (/ FRAME_HEIGHT 2))
      )
      (when (equal? game-state 'ended) 
        (send dc set-brush (make-color 0 0 0 0.5) 'solid)
        (send dc set-pen (make-color 0 0 0 0.5) 0 'solid)
        (send dc draw-rectangle 0 0 FRAME_WIDTH FRAME_HEIGHT)
        (send dc draw-text "Game over :(" (/ FRAME_WIDTH 2) (/ FRAME_HEIGHT 2))
      )
      )
    (super-new
     (paint-callback (lambda (canvas dc) (custom-paint-callback canvas dc)))
     )
    (send this set-canvas-background (make-color 0 0 100))

    (define/override (on-char event)
      (display (send event get-key-release-code))
      (newline)
      (case (send event get-key-release-code)
        ['left (set! previous-direction current-direction) (set! current-direction 'left)]
        ['right (set! previous-direction current-direction) (set! current-direction 'right)]
        ['up (set! previous-direction current-direction) (set! current-direction 'up)]
        ['down (set! previous-direction current-direction) (set! current-direction 'down)]
        ['#\space 
          (case game-state
            ['start (set! game-state 'running) (send game-timer start 10)]
            ['ended (reset-game-state) (set! game-state 'running) (send game-timer start 10)]
          )
        ]
        
        )
      )
     )
    )

(define game-state 'start)

(define game-timer
  (new timer%
       [notify-callback
        (lambda ()
          ;(send spaceship move)
          ;(display (/ FRAME_WIDTH 4))
          ;limits are placed here
          (case current-direction
            ['left (unless (< (send spaceship get-x) 0)
                    (send spaceship set-x-position! (- (send spaceship get-x) SPEED))
                     )
                    ]
            ['right (unless (> (send spaceship get-x) 400)
                      (send spaceship set-x-position! (+ (send spaceship get-x) SPEED))
                      )
                    ]
            ['up  (unless (< (send spaceship get-y) 0)
                    (send spaceship set-y-position! (- (send spaceship get-y) SPEED))
                    )]
            ['down (unless (> (send spaceship get-bottom-y) FRAME_HEIGHT)
                     (send spaceship set-y-position! (+ (send spaceship get-y) SPEED))
                     )
                   ]
            )
          ;this part makes it so when you click in the current direction, vehicle goes to max speed
          (cond
            [(equal? current-direction previous-direction)
             (case current-direction
            ['left (unless (< (send spaceship get-x) 0)
                    (send spaceship set-x-position! (- (send spaceship get-x) MAX_SPEED))
                     )
                    ]
            ['right (unless (> (send spaceship get-x) 400)
                      (send spaceship set-x-position! (+ (send spaceship get-x) MAX_SPEED))
                      )
                    ]
            ['up  (unless (< (send spaceship get-y) 0)
                    (send spaceship set-y-position! (- (send spaceship get-y) MAX_SPEED))
                    )
                  ]
            ['down (unless (> (send spaceship get-bottom-y) FRAME_HEIGHT)
                     (send spaceship set-y-position! (+ (send spaceship get-y) MAX_SPEED))
                     )
                   ]
            )
             ]
            )
          (for ([obstacle obstacle-list])
            (send obstacle move-obstacle)
            (when (< (get-field x-pos obstacle) 0)
            (send obstacle move-to-far)
              #|
              (if (equal? (get-field square-color square) 'red)
            (if (= 1 (random 2))
                (send square set-x-position! FIRST_LANE)
                (send square set-x-position! SECOND_LANE)
                )
            (if (= 1 (random 2))
                (send square set-x-position! THIRD_LANE)
                (send square set-x-position! FOURTH_LANE)
                )
            )|#
              (case (get-field obstacle-color obstacle)
                ['red (send obstacle set-y-position! (+ (random 330) 30))]
                ['yellow (send obstacle set-y-position! (+ (random 330) 30))]
                ['orange (send obstacle set-y-position! (+ (random 330) 30))]
                )
            )
            )
          (when (for/or ([obstacle obstacle-list]) (did-collide obstacle spaceship))
            (define current_hp health_points)
            (set! health_points (- current_hp 1))
            (cond
              [(= health_points 0) (send game-timer stop) (set! game-state 'ended)]
            )
            ;(set! game-state 'ended)
            )
              
          
          ;(set! current-direction 'neutral
          (send main-canvas refresh-now)
          )
        ]
        ;[interval 1000]
       )
  
  )



;(send game-timer start 10)


(define main-canvas
  (new game-canvas% [parent main-frame])
  )

(define spaceship (new ship%))
(define red_obstacle (new obstacle% [obstacle-color 'red]))
(send red_obstacle set-x-position! FRAME_WIDTH)
(send red_obstacle set-y-position! (+ 0 (+ (random 330) 30)))

(define orange_obstacle (new obstacle% [obstacle-color 'orange]))
(send orange_obstacle set-x-position! FRAME_WIDTH)
(send orange_obstacle set-y-position! (+ 0 (+ (random 330) 30)))

(define yellow_obstacle (new obstacle% [obstacle-color 'yellow]))
(send yellow_obstacle set-x-position! FRAME_WIDTH)
(send yellow_obstacle set-y-position! (+ 0 (+ (random 330) 30)))


(define obstacle-list (vector  red_obstacle yellow_obstacle orange_obstacle))
#|(define red_obstacle (new obstacle% [obstacle-color 'red]))
(send red_obstacle set-x-position! 1000)|#




;(send spaceship set-left-x-position! 10)
;(send spaceship set-left-x-position! 10)
;(send spaceship set-right-x-position! 10)
(send spaceship set-x-position! START_X)
(send spaceship set-y-position! START_Y)

(define (did-collide obstacle ship)
  (define obstacle-left-x (send obstacle get-left-x))
  (define obstacle-right-x (send obstacle get-right-x))
  (define obstacle-bottom-y (send obstacle get-bottom-y))
  (define obstacle-top-y (send obstacle get-top-y))

  (define ship-left-x (send ship get-left-x))
  (define ship-right-x (send ship get-right-x))
  (define ship-bottom-y (send ship get-bottom-y))
  (define ship-top-y (send ship get-top-y))

  (and
   (< obstacle-left-x ship-right-x)
   (> obstacle-right-x ship-left-x)
   (< obstacle-top-y ship-bottom-y)
   (> obstacle-bottom-y ship-top-y)
   )
  )

(define (reset-game-state)
  (send spaceship reset-position)
  (send red_obstacle move-to-far)
  (send orange_obstacle move-to-far)
  (send yellow_obstacle move-to-far)
)

(send main-canvas refresh-now)

(send main-frame show #t)

;(define game-state 'paused)