#lang sicp

;; 方針
;; cell が長さ 1 だったのを n に拡張。
;; acuire と release の時に cell を走査して状態を変更できる cell が見つかったら状態を変更する

;; 基本ツール
(define (clear! cell)
  (set-car! cell #f))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

(define (guard-positive n)
  (if (<= n 0)
      (error "Semaphore size must be positive" n)
      'ok))

(define (make-cell-list k)
  (if (= k 0)
      '()
      (cons (list #f) (make-cell-list (- k 1)))))

(define (make-mutex n)
  (guard-positive n)
  (let ((cells (make-cell-list n)))
    (define (acquire)
      (define (try cells)
        (cond ((null? cells) #f)
              ((not (test-and-set! (car cells))) #t)
              (else (try (cdr cells)))))
      (if (try cells)
          'ok
          (acquire)))
    (define (release)
      (define (free cells)
        (cond ((null? cells) #f)
              ((car (car cells))
               (clear! (car cells))
               #t)
              (else (free (cdr cells)))))
      (if (free cells)
          'ok
          (error "Mutex release without prior acquire")))
    (define (dispatch m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))
            (else (error "Unknown request -- MUTEX" m))))
    dispatch))

;; (a) mutex を使ったセマフォ
(define (make-semaphore-with-mutex n)
  (guard-positive n)
  (let ((mutex (make-mutex n)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

;; (b) test-and-set! を直接使ったセマフォ
(define (make-semaphore-with-test-and-set n)
  (guard-positive n)
  (let ((cells (make-cell-list n)))
    (define (acquire)
      (define (try cells)
        (cond ((null? cells) #f)
              ((not (test-and-set! (car cells))) #t)
              (else (try (cdr cells)))))
      (if (try cells)
          'acquired
          (acquire)))
    (define (release)
      (define (free cells)
        (cond ((null? cells) #f)
              ((car (car cells))
               (clear! (car cells))
               #t)
              (else (free (cdr cells)))))
      (if (free cells)
          'released
          (error "Semaphore release without acquire")))
    (lambda (p)
      (define (serialized-p . args)
        (acquire)
        (let ((val (apply p args)))
          (release)
          val))
      serialized-p)))
