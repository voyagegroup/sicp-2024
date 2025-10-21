#lang sicp

(define (make-queue) (cons '() '()))

(define (front-ptr queue) (car queue))


(define (rear-ptr queue) (cdr queue))


(define (set-front-ptr! queue item) (set-car! queue item))


(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr!  queue new-pair)
          queue)
        (begin
          (set-cdr! new-pair (front-ptr queue)) ; 先頭にリンク
          (set-front-ptr! queue new-pair)       ; 先頭を更新
          queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-agenda) (list 0))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

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


(define (make-time-segment time queue)
  (cons time queue))


(define (segment-time s) (car s))


(define (segment-queue s) (cdr s))

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



(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))


(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))


(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

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

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1))
      1
      0))

(define (logical-or s1 s2)
  (if (or (= s1 1) (= s2 1))
      1
      0))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)



(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; 3.32
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures)) ; and-gateのoutputを変更する手続きがここで実行される
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure) ; outputを再計算する手続きをa1に登録
  (add-action! a2 and-action-procedure)
  'ok)


;; FIFO版とLIFO版の insert-queue! を用意し、実験ごとに差し替える
(define (insert-queue!-fifo queue item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr!  queue new-pair)
          queue)
        (begin
          (set-cdr! (rear-ptr queue) new-pair) ; 末尾に連結
          (set-rear-ptr! queue new-pair)       ; 末尾ポインタ更新
          queue))))

(define (insert-queue!-lifo queue item)
  (let ((new-pair (cons item '())))
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr!  queue new-pair)
          queue)
        (begin
          (set-cdr! new-pair (front-ptr queue)) ; 先頭に挿入(スタック)
          (set-front-ptr! queue new-pair)
          queue))))

;; 実験シナリオ:
;; 初期 a=0, b=1 で and-gate を接続 → 初期イベントを消化
;; 同一時刻に a:0→1 を先に、b:1→0 を後に発生させる
;; これにより t+delay の箱に [SetOut1][SetOut0] の順で2件が並ぶ
(define (run-and-swap use-lifo?)
  ;; insert-queue! の差し替え(トップレベル束縛を更新)
  (set! insert-queue! (if use-lifo? insert-queue!-lifo insert-queue!-fifo))
  ;; アジェンダをリセット
  (set! the-agenda (make-agenda))

  ;; 配線とゲート
  (define a   (make-wire))
  (define b   (make-wire))
  (define out (make-wire))

  ;; 初期値を設定
  (set-signal! a 0)
  (set-signal! b 1)
  (and-gate a b out)

  ;; 接続直後にスケジュールされた初期イベント(t = and-gate-delay)を消化
  (propagate)

  ;; 同一時刻(現在時刻)で 0,1 → 1,0 に入れ替え(順序に注意:a→b)
  (set-signal! a 1) ; これが先に SetOut1 を t+delay に追加
  (set-signal! b 0) ; 後から SetOut0 を t+delay に追加

  ;; 2件の出力更新イベントを実行
  (propagate)

  ;; 最終出力を返す
  (get-signal out))

;; 実行例(FIFOとLIFOで比較)
(display "FIFO最終出力: ")
(display (run-and-swap #f)) ; 期待: 0
(newline)

(display "LIFO最終出力: ")
(display (run-and-swap #t)) ; 期待: 1(誤った最終値が残る例)
(newline)

; propagateをしたときにLIFOの場合、agendaに後から追加されたイベントが先に実行される
; (set-signal! a 1) によってoutに1をセットするイベントが追加される
; その後 (set-signal! b 0) によってoutに0をセットするイベントが追加される
; LIFOの場合、outに0をセットするイベントが先に実行され、次にoutに1をセットするイベントが実行される
; そのため最終的にoutの値は1となり、誤った結果となる