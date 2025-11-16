#lang sicp

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

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

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'make-wire-done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    ; (define (accept-action-procedure! proc)
    ;   (set! action-procedures (cons proc action-procedures)))


    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      (begin
        (display 'call-each-done)
        (newline))
      (begin
        ((car procedures))
        (display "in agenda: ")
        (display (car procedures))
        (newline)
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))


(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))


(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      (begin
        (newline)
        'propagate-done
      )
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))


(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'invert-ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (or (= s1 0) (= s1 1))
              (or (= s2 0) (= s2 1))) 0)
        (else (error "Invalid signal" (list s1 s2)))))

(define (logical-or s1 s2)
  (cond ((and (or (= s1 0) (= s1 1))
              (or (= s2 0) (= s2 1)))
         (if (and (= s1 0) (= s2 0)) 0 1))
        (else (error "Invalid signal" (list s1 s2)))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'and-gate-ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'or-gate-ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'half-adder-ok))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(display the-agenda)
(newline)

(set-signal! input-1 1)

(display the-agenda)
(newline)

(propagate)

(set-signal! input-2 1)

(propagate)

; そのままの時の出力

; sum 0  New-value = 0
; carry 0  New-value = 0
; half-adder-ok

; in agenda: #<procedure:...Chap3/3.31/noza.rkt:204:2> and-action
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:215:2> or-action
; call-each-done
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:204:2> and-action
; call-each-done
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:204:2> and-action
; call-each-done
;
; sum 8  New-value = 1
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:101:15> probe の lambda
; call-each-done
;
; propagate-done
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:204:2> and-action
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:215:2> or-action
; call-each-done
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:177:2> invert
;
; carry 11  New-value = 1
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:101:15> probe の lambda
; call-each-done
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:204:2> and-action
; call-each-done
;
; sum 16  New-value = 0
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:101:15> probe の lambda
; call-each-done
;
; propagate-done

; ----------------------------------------- ;

; 変えた時の応答

; half-adder-ok
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:204:2> and-action
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:215:2> or-action
; call-each-done
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:204:2> probe の lambda
; call-each-done

; propagate-done
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:204:2> and-action
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:215:2> or-action
; call-each-done
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:177:2> probe の lambda

; carry 11  New-value = 1
; in agenda: #<procedure:...Chap3/3.31/noza.rkt:101:15> probe の lambda
; call-each-done

; propagate-done

; 解答
; 新しく追加したアクションに現在の入力の値を反映するようにするため。
; ワイヤの初期値は 0 であるが、すでに 1 になっている可能性があるのでそれを反映させるために必要
; 半加算器の場合は、 E の初期値は 1 になっているべきだが、ワイヤの作成時には 0 のため正しい値が反映されない
