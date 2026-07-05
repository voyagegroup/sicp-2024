クエリのパターンを現在の具体化したものを歴史に入れ、現在のクエリがそれと同じであるかをチェックすると良い。
本文の例で言うと以下のようになる。

```
(married Mickey ?who)
(married ?who Mickey)
(married Mickey ?who) ;ここで同じであると気づく。
```