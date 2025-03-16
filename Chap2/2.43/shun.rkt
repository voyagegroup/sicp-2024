; 2.42
(flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1)))

; 2.43
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

; 2.42のやり方をTとすると、2.43T^T乗で増える。
; 2.43では毎回enumerate-intervalで1からboard-sizeまでのリストを作成しているので、その数それぞれに対してqueen-colsを呼び出しているため。
; 2.42ではqueen-colsの呼び出しがboardsize回だが、2.43ではboardsize^k回呼び出される。