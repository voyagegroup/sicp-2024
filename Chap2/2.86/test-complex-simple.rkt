#lang racket

(require "noza.rkt")

(displayln "=== 複素数の実部・虚部にタグ付きデータを使用できることの確認 ===")
(newline)

;; 1. scheme-numberを実部・虚部に使う
(displayln "1. scheme-numberを実部・虚部に使う複素数:")
(define s1 (make-scheme-number 3))
(define s2 (make-scheme-number 4))
(define c1 (make-complex-from-real-imag s1 s2))
(displayln (format "  作成: 3 + 4i"))
(displayln (format "  内部表現: ~a" c1))
(displayln "  → 実部・虚部共にscheme-numberタグ付き")
(newline)

;; 2. rationalを実部・虚部に使う
(displayln "2. rationalを実部・虚部に使う複素数:")
(define r1 (make-rational 1 2))  ; 1/2
(define r2 (make-rational 3 4))  ; 3/4
(define c2 (make-complex-from-real-imag r1 r2))
(displayln (format "  作成: 1/2 + 3/4i"))
(displayln (format "  内部表現: ~a" c2))
(displayln "  → 実部・虚部共にrationalタグ付き")
(newline)

;; 3. 混合型（scheme-numberとrational）
(displayln "3. scheme-numberとrationalを混ぜた複素数:")
(define c3 (make-complex-from-real-imag s1 r2))  ; 3 + 3/4i
(displayln (format "  作成: 3 + 3/4i"))
(displayln (format "  内部表現: ~a" c3))
(displayln "  → 実部はscheme-number、虚部はrational")
(newline)

(define c4 (make-complex-from-real-imag r1 s2))  ; 1/2 + 4i
(displayln (format "  作成: 1/2 + 4i"))
(displayln (format "  内部表現: ~a" c4))
(displayln "  → 実部はrational、虚部はscheme-number")
(newline)

;; 4. 基本演算のテスト
(displayln "4. タグ付きデータを含む複素数の演算:")
(displayln "  (3+4i) + (1/2+3/4i):")
(define sum (add c1 c2))
(displayln (format "    結果: ~a" sum))
(displayln "    → 結果も正しくタグ付きrationalで表現される")
(newline)

(displayln "  (3+4i) - (1/2+3/4i):")
(define diff (sub c1 c2))
(displayln (format "    結果: ~a" diff))
(newline)

;; 5. dropの動作確認（虚部が0の場合）
(displayln "5. dropの動作確認:")
(define c5 (make-complex-from-real-imag s1 (make-scheme-number 0)))
(displayln (format "  3 + 0i の内部表現: ~a" c5))
(displayln (format "  dropの結果: ~a" (drop c5)))
(displayln "  → 虚部が0なのでscheme-numberにdropされる")
(newline)

(define c6 (make-complex-from-real-imag r1 (make-rational 0 1)))
(displayln (format "  1/2 + 0i の内部表現: ~a" c6))
(displayln (format "  dropの結果: ~a" (drop c6)))
(displayln "  → 虚部が0なのでrationalにdropされる")
(newline)

(displayln "=== 結論 ===")
(displayln "複素数の実部・虚部に以下のタグ付きデータを使用できることを確認:")
(displayln "- scheme-number型")
(displayln "- rational型")
(displayln "- 両者の混合")
(displayln "また、演算結果も適切にタグ付きデータとして扱われることを確認しました。")