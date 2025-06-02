#lang racket

   ;; システムの他の部分へのインターフェース
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; complex で定義されている手続きは、add, sub, mul, div, make-from-real-imag, make-from-mag-ang のみです。
; (magnitude z) は polar と rectangular のそれぞれのパッケージで持っているので、z から呼ぶためには complex パッケージに追加する必要がある

; z = 3 + 4i とする
; (magnitude (complex rectangular 3 . 4))
; ((apply-generic 'magnitude (complex rectangular 3 . 4))
; ((get 'magnitude (complex)) (rectangular 3 . 4))
; (magnitude (rectangular 3 . 4))
; ((apply-generic 'magnitude (rectangular 3 . 4)))
; ((get 'magnitude (rectangular)) (3 . 4))
; (magnitude (3 . 4))
; 5

なので apply-generic は 2 回呼ばれる
