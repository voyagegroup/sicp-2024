#lang racket

; quotient は整数の商を返す。

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
　
; a. リストを半分にしながら、それぞれを木のノードにしていく
; リストの長さが0になるまで再帰的に続ける。
; 順序は半分にした左側を優先的に木にする。

; b. list->tree の計算量は O(n) になる。
; リストの長さが０になるまで再帰的に続けるため、すべての要素を１回ずつ訪れる。
; 木の作成は O(1) になる。
; よって、O(n) になる。
