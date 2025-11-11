#lang sicp

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

(transfer A B 10)

#|
正しくないと考える。
本質的な違いは、balanceへのアクセスが存在してないということ。
交換の場合は、「残高読みとり→計算→更新」のプロセス。
移動の場合は、「引数のamountを元に更新」のみ。
これによって、並列処理をされたとしても、他の処理に影響を受けない。
|#
