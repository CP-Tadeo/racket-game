#lang racket/gui

(provide obstacle%)
(define obstacle%
  (class object%
    (init-field [obstacle-color 'pink])

    (define obstacle-bitmap
      (read-bitmap
            (case obstacle-color
                  ['green "enemy_green.png"]
                  ['yellow "enemy_yellow.png"]
                  ['pink "enemy_pink.png"]
              )
            #:backing-scale 10
            )
      )
            
        
    (field [x-pos 0])
    (field [y-pos 0])
    (field [obs-speed 4])
    (super-new)

    (define/public (draw dc)
      (send dc draw-bitmap obstacle-bitmap x-pos y-pos)
      )

    (define/public (set-x-position! x-position)
      (set! x-pos (- x-position (/ (send obstacle-bitmap get-width) 2)))
      )

    (define/public (set-y-position! y-position)
      (set! y-pos (- y-position (/ (send obstacle-bitmap get-width) 2)))
      )

    (define/public (lower-obstacle)
      (set! y-pos (+ y-pos 5))
      )

    (define/public (move-obstacle)
      (set! x-pos (- x-pos obs-speed))
      )

    (define/public (move-to-top)
      (set! y-pos (- (send obstacle-bitmap get-height)))
      )

    (define/public (move-to-far)
      (set! x-pos 1500)
      )

    (define/public (get-left-x)
      (- x-pos (/ (send obstacle-bitmap get-width) 2))
    )

    (define/public (get-right-x)
      (+ x-pos (/ (send obstacle-bitmap get-width) 2))
    )

    (define/public (get-top-y)
      y-pos
      )

    (define/public (get-bottom-y)
      (+ y-pos (send obstacle-bitmap get-height))
      )
    )
  )