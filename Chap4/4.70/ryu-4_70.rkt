#lang sicp

; 1) letを使う
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

; めもadd-assertion!をつかうadd-rule-or-assertion!の説明
;    add-rule-or-assertion!はquery-driver-loopが表明と規則をデータベースに追加するのに使う. 各項目は, 適切であれば, その添字のところに格納される. またデータベースのすべての表明か規則のストリームに格納される.


; 2) 使わない
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)



#|
(define ones (cons-stream 1 ones)) は、(1, 1, 1, ... 1, ...)のような無限ストリーム

1) は、変更前の値をold-assertionsに退避をして、cons-streamに追加している。
なので、
(assertions, old-assertions) としてstreamにとうろくされる。
たとえば、Aを追加すると、old-asertionsは()なので
(A) となる
さらにassertionsにBを追加すると、old-assertionsは(A)なので
(B, A) となる。
けっかとして、(B, A) の様なストリームになる。

2) は、退避させずに追加しているため、THE-ASSERTIONSがそのまま登録される。
なので、
(assertions, THE-ASSERTIONS)となる。
たとえば、Aを追加すると、THE-ASSERTIONSは自身（assertionの値）を返すので、
(A, A, ...) となる
しかし、さらにassertionsにBを追加しようとすると、THE-ASSERTIONS が新しいストリーム自身を指すようになり、
(B, B, ...) となる。

なので、let束縛の目的 は set! 前の値を固定するために存在している。
|#