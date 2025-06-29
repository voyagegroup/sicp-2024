#lang racket

(require "noza.rkt")

;; テストケース
(displayln "=== 3つ以上の引数のテスト ===")

;; 1. 同じ型の3つの引数
(displayln "\n1. 同じ型の3つの引数（scheme-number）:")
(define n1 (make-scheme-number 10))
(define n2 (make-scheme-number 20))
(define n3 (make-scheme-number 30))
(displayln (add n1 n2 n3)) ; 60が期待される
(displayln (mul n1 n2 n3)) ; 6000が期待される

;; 2. 異なる型の3つの引数（型強制変換が必要）
(displayln "\n2. 異なる型の3つの引数（型強制変換）:")
(define r1 (make-rational 1 2))
(define c1 (make-complex-from-real-imag 3 4))
(displayln "scheme-number, scheme-number, rational:")
(displayln (add n1 n2 r1)) ; rational型に変換されて計算される、期待値は (rational 61 . 2)

;; 3. 2つの引数（従来通り動作することを確認）
(displayln "\n3. 2つの引数（従来通り動作）:")
(displayln (add n1 n2)) ; 30が期待される
(displayln (mul n1 n2)) ; 200が期待される

;; 4. 4つの引数
(displayln "\n4. 4つの引数:")
(define n4 (make-scheme-number 40))
; (displayln (add n1 n2 n3 n4)) ; 100が期待される（scheme-numberに定義がない場合はエラー）

;; 5. 入力の順序を入れ替えるテスト
(displayln "\n6. 入力の順序を入れ替えるテスト:")
(displayln (add n1 r1 n2)) ; rational型に変換されて

;; 6. 異なる3つの型の差
(displayln "\n7. 異なる3つの型の差:")
(displayln (sub n1 n2 r1)) ; rational型に変換されて計算される、期待値は (rational -21 . 2)
