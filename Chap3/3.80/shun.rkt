#lang sicp

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (delay exp)
  (memo-proc (lambda () exp)))

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

(define (display-stream s)
  (stream-for-each display-line s))

(define (take-stream s n)
  (if (or (zero? n) (stream-null? s))
      '()
      (cons (stream-car s)
            (take-stream (stream-cdr s) (- n 1)))))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))


(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2 1))

(define (find-divisor n test-divisor count)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1) (+ count 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

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

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (define x
    (cons-stream 1 (scale-stream (mul-series (stream-cdr s) x) -1))) ; x = 1 - sr * x
  x)

(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))

(define (average n1 n2)
  (/ (+ n1 n2) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit s tolelance)
  (let ((first (stream-car s))
        (second (stream-car (stream-cdr s))))
    (if (<= (abs (- first second)) tolelance)
        second
        (stream-limit (stream-cdr s) tolelance)))
  )


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

(define (in2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (in2-summands (+ n 1)))))

(define in2-stream
  (scale-stream (partial-sums (in2-summands 1)) 1))



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

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (w1 (weight (car s1car) (cadr s1car)))
                (w2 (weight (car s2car) (cadr s2car))))
           (cond ((< w1 w2)
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> w1 w2)
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream s1car
                               (cons-stream s2car
                                            (merge-weighted (stream-cdr s1)
                                                            (stream-cdr s2)
                                                            weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (sum-square i j)
  (+ (* i i) (* j j)))

(define (sum-square-pair p)
  (sum-square (car p) (cadr p)))

(define sum-squares
  (weighted-pairs integers integers sum-square))

(define (stream-cadr stream) (stream-car (stream-cdr stream)))

(define (stream-sum-3way s)
  (let* ((p1 (stream-car s))
         (p2 (stream-car (stream-cdr s)))
         (p3 (stream-car (stream-cdr (stream-cdr s))))
         (w1 (sum-square-pair p1))
         (w2 (sum-square-pair p2))
         (w3 (sum-square-pair p3)))
    (if (and (= w1 w2) (= w2 w3))
        (cons-stream (list w1 p1 p2 p3)
                     (stream-sum-3way (stream-cdr (stream-cdr s))))
        (stream-sum-3way (stream-cdr s)))))

(define (constant-stream k)
  (scale-stream ones k))

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt))))
  )


; 3.80
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1.0 C)))
    (define diL (add-streams (scale-stream vC (/ 1 L)) (scale-stream iL (- (/ R L)))))
    (cons vC iL)))


(define st-80 ((RLC 1 1 0.2 0.1) 10 0))