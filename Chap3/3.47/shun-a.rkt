#lang sicp

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

; a

(define (make-semaphore n)
  (let ((mutex (make-mutex))
        (count 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire) ; countへのアクセスの前にロックを取る
             (if (< count n)
                 (begin
                   (set! count (+ count 1))
                   (mutex 'release))
                 (begin
                   (mutex 'release)
                   (the-semaphore 'acquire))))
            ((eq? m 'release)
             (mutex 'acquire)
             (set! count (- count 1))
             (mutex 'release))))
    the-semaphore))