## 明白な振分けを持つ汎用演算
同じ名前の手続きがあればリネームした上で、別の名前になるように関数を定義する

## データ主導流
型を追加したいときは新しくパッケージを定義する。
演算を追加したいときは適切な型の中に演算を追加する

## メッセージパッシング流
型を追加したいときは新しくデータオブジェクトを追加する。
演算を追加したいときはデータオブジェクトの中に追加する。必要であれば引数を追加する。

## 新しい型が絶えず追加されるシステムにはどの方法が最も適切か
データ主導かメッセージパッシングであれば良い。

## 新しい演算が絶えず追加されるシステム
データ主導が良い。特に何かを気にすることなく型の中に演算を追加できる。
メッセージパッシングでは一引数の汎用演算しかできないので複素数同士の足し算などリッチなことはできない。
