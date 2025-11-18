#lang sicp
#|
acc1, acc2があるとする
1つのaccのロックをとり、動的に振り込み先をきめるような処理があったとする。

ok: 先にとったロックのid < 動的に決まったaccountのid
acc1のロックをとる → なんらかの処理 → acc2に振り込むことにする → (exchange acc1 acc2) → ok

ng: 先にとったロックのid > 動的に決まったaccountのid
acc2のロックをとる → なんらかの処理 → acc1に振り込むことにする → (exchange acc2 acc1) → デッドロック

|#