# 解答

flatten-stream が delay を使うのは、外側のストリームの残りを平坦化する処理を、実際に必要になるまで遅らせるためである。

元の定義は次のようになっている。

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

この定義では、まず外側のストリームの先頭にあるストリームから答えを取り出せる。

外側のストリームの残りに対する

(flatten-stream (stream-cdr stream))

は delay されているため、後続の答えが必要になるまで評価されない。

一方、次の定義では問題が起こる。

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
       (stream-car stream)
       (flatten-stream (stream-cdr stream)))))

Schemeは作用的順序で評価するため、interleave を呼び出す前に、その第二引数である

(flatten-stream (stream-cdr stream))

を評価しなければならない。

しかし、その呼び出しの中でも、さらに

(flatten-stream (stream-cdr stream))

が評価される。そのため、外側のストリームが無限ストリームであれば、外側のストリームの末尾を探して再帰し続け、interleave は一度も呼び出されない。

例えば、外側のストリームが次のような無限のストリームのストリームであるとする。

(singleton-stream 1)
(singleton-stream 2)
(singleton-stream 3)
...

期待する平坦化の結果は次のようになる。

1
2
3
...

しかし delay を使わない定義では、最初の 1 を返す前に、外側のストリームの残りすべてを平坦化しようとする。

外側のストリームには終わりがないため、この計算は終了せず、最初の答えさえ得られない。

delay を使った定義では、まず

(stream-car stream)

で得られる最初のストリームから答えを返し、外側のストリームの残りは、さらに答えが要求されたときに初めて平坦化される。

したがって、delay の目的は、無限のストリームのストリームを一度にすべて平坦化しようとするのを防ぎ、必要な部分だけを順番に計算できるようにすることである。
