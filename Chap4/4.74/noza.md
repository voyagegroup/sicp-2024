# 解答 b.

この変更によって、negate、lisp-value、find-assertions の動作は変わらない。

これらの手続きで stream-flatmap に渡される手続きは、各フレームに対して常に次のどちらかを返す。

* 条件を満たさない場合は空ストリーム the-empty-stream
* 条件を満たす場合は、そのフレームだけを含む単一ストリーム (singleton-stream frame)

したがって、stream-map の結果は、空ストリームまたは単一ストリームを要素とするストリームになる。

通常の flatten-stream は、各部分ストリームを interleave-delayed によって交互に取り出す。しかし、ここでは各部分ストリームに含まれる要素は最大でも一つなので、複数の要素を公平に交互に取り出す必要がない。

simple-flatten は、まず空ストリームを stream-filter で取り除き、残った単一ストリームから stream-car によって唯一の要素を取り出す。

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter
               (lambda (s)
                 (not (stream-null? s)))
               stream)))

そのため、元の flatten-stream を用いた場合と、simple-flatten を用いた場合で得られるフレームの順序と内容は同じになる。
