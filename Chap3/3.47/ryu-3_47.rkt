#lang sicp



; a. 相互排除器を使って
(define (make-semaphore n)
  (let ((count n))
    (let ((mutex (make-mutex)))      
      (define (acquire-semaphore)
        (mutex 'acquire)
        (if ( > count 0)
            (begin
              (set! count (- count 1))
              (mutex 'release))
            (begin
              (mutex 'release)
              (acquire-semaphore)))) ; retry
      (define (release-semaphore)
        (mutex 'acquire)
        (set! count (+ count 1))
        (mutex 'release))
      (define (semaphore m)
        (cond ((eq? m 'acquire)
               (acquire-semaphore))
              ((eq? m 'release)
               (release-semaphore))))
      semaphore)))

; b. test-and-set!を使って
(define (make-semaphore-b n)
  (let ((count n))
    ; (let ((mutex (make-mutex)))
    (let ((cell (list false)))            
      (define (acquire-semaphore)
        #|(mutex 'acquire)
        (if ( > count 0)
            (begin
              (set! count (- count 1))
              (mutex 'release))
            (begin
              (mutex 'release)
              (acquire-semaphore)))) ; retry
|#
        (if (test-and-set! cell)
            (acquire-semaphore)
                    (if ( > count 0)
            (begin
              (set! count (- count 1))
              (clear! cell))
            (begin
              (clear! cell)
              (acquire-semaphore))))) ; retry
      
      (define (release-semaphore)
        (if (test-and-set! cell)
            (release-semaphore)
            (begin
              (set! count (+ count 1))
              (clear! cell))))
      (define (semaphore m)
        (cond ((eq? m 'acquire)
               (acquire-semaphore))
              ((eq? m 'release)
               (release-semaphore))))
      semaphore)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

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


#|
(define (test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
         true
         (begin (set-car! cell true)
                false)))))
|#

