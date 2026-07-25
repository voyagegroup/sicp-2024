# 解答

`unique` は、入力された各フレームについて部分質問を評価し、その結果がちょうど一つだけ存在するときに成功する特殊形式である。

```scheme
(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((result-stream
            (qeval (unique-query operands)
                   (singleton-stream frame))))
       (if (and (not (stream-null? result-stream))
                (stream-null? (stream-cdr result-stream)))
           result-stream
           the-empty-stream)))
   frame-stream))

(define (unique-query operands)
  (car operands))

(put 'unique 'qeval uniquely-asserted)
```

`qeval` には、現在のフレームだけを含む単一ストリームを渡す。返された `result-stream` を次のように判定する。

* 空ストリームなら、部分質問を満たす項目がないため失敗する。
* 先頭要素を取り除いたストリームが空でなければ、項目が二つ以上あるため失敗する。
* 空ではなく、先頭要素を取り除くと空になるなら、項目が一つだけなので成功する。

成功時には元の `frame` ではなく `result-stream` を返す。これにより、部分質問の評価で追加された変数束縛が `unique` の結果にも残る。各入力フレームから返された結果は `stream-flatmap` によって一つのストリームにまとめられる。
