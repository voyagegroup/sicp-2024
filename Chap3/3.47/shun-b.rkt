#lang sicp

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-semaphore n)
  (let ((count 0)              
        (lock  (list false)))                       
    (define (acquire-lock)
      (if (test-and-set! lock)
          (acquire-lock)))      ; retry
    (define (release-lock)
      (clear! lock))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (acquire-lock)
             (if (< count n)
                 (begin
                   (set! count (+ count 1))
                   (release-lock))
                 (begin
                   (release-lock)
                   (the-semaphore 'acquire))))
            ((eq? m 'release)
             (acquire-lock)
             (set! count (- count 1))
             (release-lock))))
    the-semaphore))
