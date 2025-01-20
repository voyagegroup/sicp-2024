#lang racket

(define (make-interval a b)
    (cons a b))

(define (lower-bound i)
    (car i))

(define (upper-bound i)
    (cdr i))

(define (mul-interval x y)
    (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond
        ; 両方の区間が正の数の場合
        ((and (>= lx 0) (>= ux 0) (>= ly 0) (>= uy 0))
            (make-interval (* lx ly) (* ux uy)))
        ; xが正の数、yが負の数の場合
        ((and (>= lx 0) (>= ux 0) (<= ly 0) (<= uy 0))
            (make-interval (* ux ly) (* lx uy)))
        ; xが負の数、yが正の数の場合
        ((and (<= lx 0) (<= ux 0) (>= ly 0) (>= uy 0))
            (make-interval (* lx uy) (* ux ly)))
        ; 両方の区間が負の数の場合
        ((and (<= lx 0) (<= ux 0) (<= ly 0) (<= uy 0))
            (make-interval (* ux uy) (* lx ly)))
        ; xが正の数、yが正負の数を含む場合
        ((and (>= lx 0) (>= ux 0) (<= ly 0) (>= uy 0))
            (make-interval (* ux ly) (* ux uy)))
        ; xが負の数、yが正負の数を含む場合
        ((and (<= lx 0) (<= ux 0) (<= ly 0) (>= uy 0))
            (make-interval (* lx uy) (* lx ly)))
        ; xが正負の数を含む、yが正の数の場合
        ((and (<= lx 0) (>= ux 0) (>= ly 0) (>= uy 0))
            (make-interval (* lx uy) (* ux uy)))
        ; xが正負の数を含む、yが負の数の場合
        ((and (<= lx 0) (>= ux 0) (<= ly 0) (<= uy 0))
            (make-interval (* ux ly) (* lx ly)))
        ; 両方の区間が正負の数を含む場合
        ((and (<= lx 0) (>= ux 0) (<= ly 0) (>= uy 0))
            (make-interval
                (min (* lx uy) (* ux ly))
                (max (* lx ly) (* ux uy)))))))
