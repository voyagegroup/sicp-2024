#lang racket

(define (call-the-cops)
  (display "警察を呼んでいます！不正アクセスが検出されました！")
  (newline))

(define (make-account balance password)
  (define incorrect-password-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd m)
    (if (eq? pwd password)
        (begin
          (set! incorrect-password-count 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
        (begin
          (set! incorrect-password-count (+ incorrect-password-count 1))
          (if (= incorrect-password-count 7)
              (lambda (x) (call-the-cops))
              (lambda (x) "Incorrect password")))))
  dispatch)

;; テスト用のコード
(define acc (make-account 100 'secret-password))

;; 正しいパスワードでwithdraw
(display "正しいパスワードでwithdraw: ")
(display ((acc 'secret-password 'withdraw) 40))
(newline)

;; 新しいアカウントで連続7回の不正アクセスをテスト
(display "\n連続7回の不正アクセステスト:\n")
(display "1回目: ")
(display ((acc 'wrong-password 'withdraw) 10))
(newline)
(display "2回目: ")
(display ((acc 'wrong-password 'deposit) 20))
(newline)
(display "3回目: ")
(display ((acc 'wrong-password 'withdraw) 30))
(newline)
(display "4回目: ")
(display ((acc 'wrong-password 'withdraw) 40))
(newline)
(display "5回目: ")
(display ((acc 'wrong-password 'deposit) 50))
(newline)
(display "6回目: ")
(display ((acc 'wrong-password 'withdraw) 60))
(newline)
(display "7回目 (警察が呼ばれるはず): ")
((acc 'wrong-password 'withdraw) 70)

;; 正しいパスワードでアクセスしてカウンタがリセットされることを確認
(display "\n正しいパスワードでアクセス（カウンタリセット）: ")
(display ((acc 'secret-password 'withdraw) 10))
(newline)

;; 再度不正アクセスをテスト（カウンタがリセットされているか確認）
(display "\n再度不正アクセス（1回目）: ")
(display ((acc 'wrong-password 'withdraw) 10))
(newline)
