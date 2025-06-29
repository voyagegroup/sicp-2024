#lang racket

(require "noza.rkt")

(displayln "=== 複素数の実部・虚部にタグ付きscheme-numberとrationalを使えることを確認 ===")
(newline)

;; 1. scheme-numberを実部・虚部に使う
(displayln "1. scheme-numberを実部・虚部に使う:")
(define s1 (make-scheme-number 3))
(define s2 (make-scheme-number 4))
(define c1 (make-complex-from-real-imag s1 s2))
(displayln (format "  3 + 4i を作成: ~a" c1))
(newline)

;; 2. rationalを実部・虚部に使う
(displayln "2. rationalを実部・虚部に使う:")
(define r1 (make-rational 1 2))  ; 1/2
(define r2 (make-rational 3 4))  ; 3/4
(define c2 (make-complex-from-real-imag r1 r2))
(displayln (format "  1/2 + 3/4i を作成: ~a" c2))
(newline)

;; 3. scheme-numberとrationalを混ぜて使う
(displayln "3. scheme-numberとrationalを混ぜて使う:")
(define c3 (make-complex-from-real-imag s1 r2))  ; 3 + 3/4i
(displayln (format "  3 + 3/4i を作成: ~a" c3))
(define c4 (make-complex-from-real-imag r1 s2))  ; 1/2 + 4i
(displayln (format "  1/2 + 4i を作成: ~a" c4))
(newline)

;; 4. 演算のテスト
(displayln "4. 複素数の演算テスト:")
(displayln "  c1 + c2 (scheme-number複素数 + rational複素数):")
(define sum (add c1 c2))
(displayln (format "  (3+4i) + (1/2+3/4i) = ~a" sum))
(newline)

(displayln "  c3 * c4 (混合型複素数の掛け算):")
(define prod (mul c3 c4))
(displayln (format "  (3+3/4i) * (1/2+4i) = ~a" prod))
(newline)

;; 5. magnitude/angleの計算テスト（タグ付きデータを含む）
(displayln "5. magnitude/angleの計算テスト:")
(displayln "  scheme-number複素数 (3+4i):")
(displayln (format "    内部表現: ~a" c1))
(displayln (format "    magnitude = 5 (期待値)"))
(displayln (format "    angle = atan(4/3) ≈ 0.927 (期待値)"))
(newline)

(displayln "  rational複素数 (1/2+3/4i):")
(displayln (format "    内部表現: ~a" c2))
(displayln (format "    magnitude = √((1/2)²+(3/4)²) = √(1/4+9/16) = √(13/16) = √13/4 ≈ 0.901 (期待値)"))
(newline)

;; 6. dropの動作確認
(displayln "6. dropの動作確認:")
(define c5 (make-complex-from-real-imag s1 (make-scheme-number 0)))  ; 3 + 0i
(displayln (format "  3 + 0i を作成: ~a" c5))
(displayln (format "  dropの結果: ~a" (drop c5)))
(displayln "  (虚部が0なのでscheme-numberまでdropされるはず)")
(newline)

(define c6 (make-complex-from-real-imag r1 (make-rational 0 1)))  ; 1/2 + 0i
(displayln (format "  1/2 + 0i を作成: ~a" c6))
(displayln (format "  dropの結果: ~a" (drop c6)))
(displayln "  (虚部が0なのでrationalまでdropされるはず)")