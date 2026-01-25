#lang sicp

(define-syntax stream-cons
  (syntax-rules ()
    [(_ a b)
     (cons-stream a b)]))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-take s n)
  (cond ((or (= n 0) (stream-null? s)) '())
        ((= n 1) (list (stream-car s)))
        (else
         (cons (stream-car s)
               (stream-take (stream-cdr s) (- n 1))))))


(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define (integrate-series st)
  (define (itr s n)
    (cons-stream
     (* (/ 1 n) (stream-car s))
     (itr (stream-cdr s) (+ n 1))))
  (itr st 1))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))


(define (stream-limit s tolelance)
  (let ((first (stream-car s))
        (second (stream-car (stream-cdr s))))
    (if (<= (abs (- first second)) tolelance)
        second
        (stream-limit (stream-cdr s) tolelance)))
  )

(define (square n)
  (* n n))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (add-streams (stream-cdr s)
                (partial-sums s))))


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


(define random-init 7)

(define (rand-update x)
  (remainder (+ (* 1234 x) 567) 89))

(define (rand requests last-value)
  (let* ((request (stream-car requests))
         (new-value
          (cond ((eq? request 'generate)
                 (rand-update last-value))
                ((and (pair? request) (eq? (car request) 'reset))
                 (cadr request))
                (else
                 (error "Unknown request" request)))))
    (cons-stream new-value
                 (rand (stream-cdr requests) new-value))))

(define (list->stream xs)
  (if (null? xs)
      the-empty-stream
      (cons-stream (car xs) (list->stream (cdr xs)))))

(define requests
  (list->stream (list 'generate
                      'generate
                      (list 'reset 42)
                      'generate
                      'generate
                      'generate
                      (list 'reset 5)
                      'generate)))

(define randoms (rand requests random-init))

(stream-take randoms 8)
; (38 22 42 63 78 76 5 62)
