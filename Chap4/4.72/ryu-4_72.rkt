#lang sicp
;  (ヒント: 3.5.3節でinterleaveを使ったのはなぜか.)
; https://sicp.iijlab.net/fulltext/x353.html
;    無限のストリームを扱うには, プログラムを十分長く走らせると, すべての要素に遂にはたどり着けるということが確かな組合せの順を考える必要がある. これを達成する優美な方法は次のinterleave手続きを使うものである
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
; interleaveは二つのストリームから要素を交互にとるので, 第一のストリームが無限であっても, 第二のストリームのすべての要素は, いつかは混ぜ合されたストリームへ行く道を見つける.

(or
 (無限のquery)
 (無限じゃないquery))
; の場合、単純に連接すると、無限のqueryがおわらないと、無限じゃないqueryが読み出されない。
; 交互にすることで、1つめが無限であっても、2つめの結果が呼び出される。

