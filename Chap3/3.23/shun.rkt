#lang sicp

(define (front-ptr qs) (car qs))
(define (rear-ptr qs) (cdr qs))
(define (set-front-ptr! queue node) (set-car! queue node))
(define (set-rear-ptr! queue node) (set-cdr! queue node))

(define (make-node item)
  (cons item (cons '() '())))

(define (node-item item) (car item))
(define (node-prev-item item) (car (cdr item)))
(define (node-next-item item) (cdr (cdr item)))

(define (set-node-prev! node q)
  (set-car! (cdr node) q))

(define (set-node-next! node q)
  (set-cdr! (cdr node) q))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (node-item (front-ptr queue))))

(define (rear-queue queue)
  (if (empty-queue? queue)
      (error "REAR called with an empty queue" queue)
      (node-item (front-ptr queue))))

(define (front-insert-queue! dq item)
  (let ((new-node (make-node item)))
    (cond ((empty-queue? dq)
        (set-front-ptr! dq new-node)
        (set-rear-ptr! dq new-node)
        dq)
          (else
           (let ((old (front-ptr dq)))
             (set-node-prev! old new-node)
             (set-node-next! new-node old)
             (set-front-ptr! dq new-node)
             dq)))))

(define (rear-insert-queue! dq item)
  (let ((new-node (make-node item)))
    (cond ((empty-queue? dq)
        (set-front-ptr! dq new-node)
        (set-rear-ptr! dq new-node)
        dq)
          (else
           (let ((old (rear-ptr dq)))
             (set-node-prev! new-node old)
             (set-node-next! old new-node)
             (set-rear-ptr! dq new-node)
             dq)))))

(define (front-delete-queue! dq)
  (cond ((empty-queue? dq)
         (error "DELETE! called with an empty queue" dq))
        (else
         (let ((next (node-next-item (front-ptr dq))))
           (set-node-prev! next '())
           (set-front-ptr! dq next)))))

(define (rear-delete-queue! dq)
  (cond ((empty-queue? dq)
         (error "DELETE! called with an empty queue" dq))
        (else
         (let ((prev (node-prev-item (rear-ptr dq))))
           (set-node-next! prev '())
           (set-rear-ptr! dq prev)))))


(define (print-queue dq)
  (define (print q)
     (display (node-item q))
     (if (null? (node-next-item q))
         (display "")
         (print (node-next-item q))))
  (print (front-ptr dq)))


(define q1 (make-queue))

(front-insert-queue! q1 'a)

(front-insert-queue! q1 'b)

(rear-insert-queue! q1 'c)

(front-delete-queue! q1)

(rear-delete-queue! q1)

(print-queue q1)