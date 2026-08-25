#lang sicp
; あまり馬鹿げていない(少なくとも一回の再帰呼出しの実行が必要な)値を使い, 階乗とFibonacci計算機を机上シミュレートせよ. 実行の主要な場所でのスタックの内容を示せ.

(controller
   (assign continue (label fact-done))     ; 最終帰り番地設定
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;;nとcontinueを退避し再帰呼出しを設定する.
   ;; 再帰呼出しから戻る時after-fact}から
   ;; 計算が続行するようにcontinueを設定
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))   ; valに n(n-1)!がある
   (goto (reg continue))                   ; 呼出し側に戻る
 base-case
   (assign val (const 1))                  ; 基底の場合: 1!=1
   (goto (reg continue))                   ; 呼出し側に戻る
 fact-done)

; n=3とする

(assign continue (label fact-done))

(test (op =) (reg 3) (const 1)) ; n=3 false
(save continue) ; (fact-done) がスタック
(save 3) ; n のスタックは(3)
(assign n (op -) (reg 3) (const 1)) ; n = 2へ
(assign continue (label after-fact))
(goto (label fact-loop))

(test (op =) (reg 2) (const 1)) ; n=2 false
(save continue) ; (fact-done, after-fact) がスタック
(save 2) ; nのスタックは(3, 2)
(assign n (op -) (reg 2) (const 1)) ; n=1へ
(assign continue (label after-fact))
(goto (label fact-loop))

(test (op =) (reg 1) (const 1)) ; n=1 true
(branch (label base-case))
(assign val (const 1)) ; valに1をアサイン
(goto (reg continue)) ; continueにafter-vactが入っているので、after-factへgoto

(restore n) ; nのstackが(3, 2) 。2が取り出される
(restore continue) ; continueのstackが(fact-done, after-fact)。after-factが取り出される
(assign val (op *) (reg 2) (reg 1)) ; valはまだ1なので、val=2*1になる
(goto (reg continue)) ; after-factへ

(restore n) ; nのstackが(3)なので3が取り出される
(restore continue) ; continueのstackが(fact-done)なので、fact-doneがとりだされる
(assign val (op *) (reg 3) (reg 2)) ; val=6
(goto (reg continue)) ; fact-doneへ
; val = 6なので、出力は6となる




