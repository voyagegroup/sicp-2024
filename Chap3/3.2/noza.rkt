#lang racket

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (if (eq? x 'how-many-calls?)
        count
        (begin
          (set! count (+ count 1))
          (f x))))))

;; Test
(define s (make-monitored sqrt))

(displayln (s 100))          ; 10
(displayln (s 'how-many-calls?))  ; 1
(displayln (s 16))           ; 4
(displayln (s 'how-many-calls?))  ; 2
