# 解答

disjoin と stream-flatmap がストリームを単純に連接せず、interleave-delayed によって差し込むのは、探索を公平に進めるためである。

単純な連接では、先に置かれたストリームが無限ストリームだった場合、その要素を取り出し続けるため、後に置かれたストリームの要素には永久に到達できない。

一方、差込みを使えば、各ストリームから交互に要素を取り出すので、一つの探索経路が無限に続いても、ほかの探索経路から得られる答えを返すことができる。

例えば、自然数を表す次の表明と規則を考える。

(assert! (number zero))
(assert! (rule (number (succ ?n))
               (number ?n)))

質問

(number ?x)

は、次のような無限個の答えを生成する。

zero
(succ zero)
(succ (succ zero))
...

disjoin の場合

さらに次の表明を追加する。

(assert! (special done))

そして次の質問を行う。

(or (number ?x)
    (special ?x))

二つの選言肢の結果を単純に stream-append すると、最初の

(number ?x)

が無限ストリームを生成するため、その答えを取り出し続けることになる。

その結果、第二の選言肢

(special ?x)

から得られる

?x = done

には永久に到達できない。

一方、interleave-delayed を使えば、二つの選言肢の結果を交互に取り出すため、自然数の列が無限に続いていても、?x = done を得ることができる。

stream-flatmap の場合

次の表明と規則も考える。

(assert! (source b))
(assert! (source a))
(assert! (rule (answer a ?n)
               (number ?n)))
(assert! (answer b done))

次の and 質問では、最初の質問から ?x = a と ?x = b の二つのフレームが作られる。

(and (source ?x)
     (answer ?x ?y))

第二の質問を各フレームに適用すると、次の結果ストリームが得られる。

* ?x = a のフレームからは、number を使った無限個の答え
* ?x = b のフレームからは、?y = done という一個の答え

stream-flatmap が各結果ストリームを単純に連接すると、?x = a から生じる無限ストリームを処理し続けるため、

?x = b
?y = done

という答えには到達できない。

flatten-stream が interleave-delayed を使って結果ストリームを差し込めば、最初のフレームが無限個の答えを生成しても、後続のフレームから得られる答えも取り出すことができる。

したがって、差込みの目的は、一つの選択肢や一つのフレームが無限個の結果を生成する場合にも、それ以外の探索経路を飢餓状態にせず、すべての経路を公平に探索することである。
