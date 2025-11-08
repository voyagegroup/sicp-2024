# 解答

Louis の案では内部手続きでも `balance-serializer` を使ってロックを取ろうとしている。
この状態で、`serialized-exchange` を実行した場合、まず外部に公開された `serializer` を用いて、account1・account2 両者のロックを取る。
さらに `exchange` 手続きないで `withdraw` を実行しようとした際にさらにロックを取得しにいこうとするが、すでに `serialized-exchange` の中で account1 についてロックが取られているためロックを取得することができない。
すなわちデッドロックとなってしまい手続きが進まなくなってしまう。
