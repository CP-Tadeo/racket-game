#lang racket/gui

(provide projectile%)

(define projectile%
  (class object%
    (init-field [projectile-color 'default])

    (define projectile-bitmap
      (read-bitmap "projectile.png" #:backing-scale 1.2)
      )
        
    (field [x-pos 0])
    (field [y-pos 0])
    (field [ship-x 0])
    (field [ship-y 0])
    (field [proj-speed 15])
    (field [is-being-fired #f])
    (super-new)

    (define/public (draw dc)
      (send dc draw-bitmap projectile-bitmap x-pos y-pos)
      )

    (define/public (set-x-position! x-position)
      (set! x-pos (- x-position (/ (send projectile-bitmap get-width) 2)))
      )

    (define/public (set-y-position! y-position)
      (set! y-pos (- y-position (/ (send projectile-bitmap get-width) 2)))
      )

    (define/public (set-ship-position! ship-x-pos ship-y-pos)
      (set! ship-x ship-x-pos)
      (set! ship-y ship-y-pos)
      )

    (define/public (lower-projectile)
      (set! y-pos (+ y-pos 5))
      )

    (define/public (move-projectile)
      (set! x-pos (+ x-pos proj-speed))
      )

    (define/public (move-to-top)
      (set! y-pos (- (send projectile-bitmap get-height)))
      )

    (define/public (fire)
      (set! is-being-fired #t)
    )

    (define/public (finish-firing)
      (set! is-being-fired #f)
    )
  
    (define/public (reset-position)
      (set! is-being-fired #f)
      (set! x-pos ship-x)
      (set! y-pos ship-y)
      )

    (define/public (get-is-being-fired)
      is-being-fired
    )
    (define/public (get-left-x)
      (- x-pos (/ (send projectile-bitmap get-width) 2))
    )

    (define/public (get-right-x)
      (+ x-pos (/ (send projectile-bitmap get-width) 2))
    )

    (define/public (get-top-y)
      y-pos
      )

    (define/public (get-bottom-y)
      (+ y-pos (send projectile-bitmap get-height))
      )
    )
  )