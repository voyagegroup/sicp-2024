#lang racket

(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (interval-width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; 2つの区間、X,Yをそれぞれ幅を使って表すことにする。
; Xの中央をx-c、幅をx-wとし、Yの中央をy-c、幅をy-wとする。
; すると上限、下限はそれぞれ以下のように表せる。
; 上限: (x-c + x-w)、(y-c + y-w)
; 下限: (x-c - x-w)、(y-c - y-w)
; するとXとYの2区間の和は
; 上限: (x-c + x-w) + (y-c + y-w)
; 下限: (x-c - x-w) + (y-c - y-w)
; となる。
; この区間の幅は定義より
; (((x-c + x-w) + (y-c + y-w)) - ((x-c - x-w) + (y-c - y-w))) / 2
; = (x-w + y-w + x-w + y-w) / 2
; = 2(x-w + y-w) / 2
; = (x-w + y-w)

; このように、2区間の和の結果の幅はそれぞれの区間の幅だけで表すことができる。
; 差についても同様にして(x-w - y-w)と求めることができ、区間の幅だけで表すことができる。

; 乗算においては、仮にX, Yの上限どうしが結果の上限に、X,Yの下限どうしが結果の下限になるとして、
; 結果の区間を以下のように表す。
; 上限: (x-c + x-w) * (y-c + y-w)
; 下限: (x-c - x-w) * (y-c - y-w)
; これを区間の幅の定義に当てはめても
; (((x-c + x-w) * (y-c + y-w)) - ((x-c - x-w) * (y-c - y-w))) / 2
; =(x-c*y-c + x-w*y-c + x-w*y-w + x-w*y-w - (x-c*y-c - x-w*y-c - x-c*y-w + x-w*y-w)) / 2
; =(x-c*y-c + x-w*y-c + x-w*y-w + x-w*y-w - x-c*y-c + x-w*y-c + x-c*y-w - x-w*y-w) / 2
; =(2x-w*y-c + x-w*y-w + x-c*y-w) / 2
; となり、x-c, y-cは消えない。
; 除算も乗算を使うことから同様に、区間の幅のみで結果の幅を表すことはできない。
