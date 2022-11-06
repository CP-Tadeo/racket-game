#lang racket/gui

(provide ship%)

(define ship%
  (class object%
    (init-field [ship-color 'red])

    (define ship-bitmap
      (read-bitmap "ship.png" #:backing-scale 1.5)
      
      )

    (define moving-to 'left)

    (define x-pos 0)
    (define y-pos 0)
    (define MAX_SPEED 10)

    (define left-x-pos 0)
    (define right-x-pos 0)
    (define current-direction 'left)

    (define/public (move)
      (case moving-to
        ['left (set! x-pos (- (x-pos) 1))]
        ['right (set! x-pos (+ (x-pos) 1))]
        )
      )


    (define/public (set-left-x-position! left-x-position)
      (set! left-x-pos (- left-x-position (/ (send ship-bitmap get-width) 2)))
      )

    (define/public (set-right-x-position! right-x-position)
      (set! right-x-pos (- right-x-position (/ (send ship-bitmap get-width) 2)))
      )
    

    (define/public (set-y-position! t-position)
      (set! y-pos t-position)
      )

    (define/public (set-x-position! t-position)
      (set! x-pos t-position)
      )
    
    (define/public (reset-position)
      (set! x-pos left-x-pos)
      )
    
    (super-new)
    
    
    (define/public (draw dc)
      (send dc draw-bitmap ship-bitmap x-pos y-pos)
            )

    (define/public (get-left-x)
      (- x-pos (/ (send ship-bitmap get-width) 2))
    )

    (define/public (get-x)
      x-pos
      )

    (define/public (get-y)
      y-pos
      )

    (define/public (get-right-x)
      (+ x-pos (+ (/ (send ship-bitmap get-width) 2) 10))
    )

    (define/public (get-top-y)
      y-pos
      )

    (define/public (get-bottom-y)
      (+ (+ y-pos (send ship-bitmap get-height) 40))
      )
    )
  )