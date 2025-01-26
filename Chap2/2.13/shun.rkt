; 区間xのパーセント相対誤差を割合に直したものをa、区間yのパーセント相対誤差を割合に直したものをbとする。
; この時区間x, yはそれぞれの中央値をxc, ycとすると、
(define x
  (make-interval (- xc (/ (* xc a) 2)) (+ xc (/ (* xc a) 2))))

(define y
  (make-interval (- yc (/ (* yc b) 2)) (+ yc (/ (* yc b) 2))))

; 全てを正とすると、掛け算の式は
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval p1 p4)))

; で表される。
; これにx, yの定義を当てはめると、

(define (mul-interval x y)
  (let ((p1 (* (- xc (/ (* xc a) 2)) (- yc (/ (* yc b) 2))))
        (p4 (* (+ xc (/ (* xc a) 2)) (+ yc (/ (* yc b) 2)))))
    (make-interval p1 p4)))

; p1は展開すると以下のようになる。
; (xc - xc*a/2) * (yc - (yc*b/2))
; =xc*yc-xc*yc*b/2-xc*yc*a/2+xc*yc*a*b/4
; ここで仮定より、パーセント相対許容誤差が小さいとすると(a*b)は0と扱って無視する。
; =xc*yc-xc*yc*b/2-xc*yc*a/2
; =xc*yc - (xc*yc)(b+a)/2
; よって、下限の誤差はb+aになる。
; 同様にして上限においても
; (xc + xc*a/2) * (yc + (yc*b/2))
; =xc*yc+xc*yc*b/2+xc*yc*a/2+xc*yc*a*b/4
; (a*b)を０として、
; =xc*yc + (xc*yc)(b+a)/2
; こちらもb+aが誤差になる。