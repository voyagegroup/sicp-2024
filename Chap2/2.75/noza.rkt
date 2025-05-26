#lang racket

(define (make-from-mag-ang r a)
    (define (dispatch op)
        (cond ((eq? op 'real-part) (* r (cos a)))
              ((eq? op 'img-part) (* r (sin a)))
              ((eq? op 'magnitude) r)
              ((eq? op 'angle) a)
              (else
                (error "Unknown op -- MAKE-FROM-MAG_ANG" op))))
    dispatch)
