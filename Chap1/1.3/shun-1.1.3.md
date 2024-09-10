> 三つの数値を引数として取り、そのうち⼤きいほう  から⼆つの数値の⼆乗の和を返す⼿続きを定義せよ。 

(define (top-2-sum-square a b c) 
(cond 
(and (> a b) (> b c)) (+ (* a a) (* b b)) 
(and (> b c) (> c a)) (+ (* b b) (* c c))
(else (+ (* a a) (* c c)))
)
)