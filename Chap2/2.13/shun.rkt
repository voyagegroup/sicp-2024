; 区間xのパーセント相対誤差を割合に直したものをa、区間yのパーセント相対誤差を割合に直したものをbとする。
; この時区間x, yはそれぞれの中央値をxc, ycとすると、
(define x
  (make-interval (- xc (/ (* xc a) 2)) (+ xc (/ (* xc a) 2))))

(define y
  (make-interval (- yc (/ (* yc b) 2)) (+ yc (/ (* yc b) 2))))

; 全てを正とすると、掛け算の式は
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

で表される。
これにx, yの定義を当てはめると、

(define (mul-interval x y)
  (let ((p1 (* (- xc (/ (* xc a) 2)) (- yc (/ (* yc b) 2))))
        (p2 (* (- xc (/ (* xc a) 2)) (+ yc (/ (* yc b) 2))))
        (p3 (* (+ xc (/ (* xc a) 2)) (- yc (/ (* yc b) 2))))
        (p4 (* (+ xc (/ (* xc a) 2)) (+ yc (/ (* yc b) 2)))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

