#lang sicp

(define (front-ptr queue) (car queue))


(define (rear-ptr queue) (cdr queue))


(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 


(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue queue)
  (car queue))


(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (insert-queue item)
      (let ((new-pair (cons item '())))
    (cond ((null? front-ptr)
           (set-front-ptr! new-pair)
           (set-rear-ptr! new-pair)
           (cons front-ptr rear-ptr))
          (else
           (set-cdr! rear-ptr new-pair)
           (set-rear-ptr! new-pair)
           (cons front-ptr rear-ptr)))))
    (define (delete-queue)
      (cond ((null? front-ptr)
         (error "DELETE! called with an empty queue" front-ptr))
        (else
         (set-front-ptr! (cdr front-ptr))
         (cons front-ptr rear-ptr))))
    (define (dispatch m)
      (cond ((eq? m 'insert-queue) insert-queue)
            ((eq? m 'delete-queue) delete-queue)
            (else (error "Unknown request -- MAKE_QUEUE"
                       m))))
    dispatch))

(define q1 (make-queue))

((q1 'insert-queue) 'a)
; ((a) a)

((q1 'insert-queue) 'b)
; ((a b) b)

((q1 'delete-queue))
; ((b) b)

((q1 'delete-queue))
; (() b)