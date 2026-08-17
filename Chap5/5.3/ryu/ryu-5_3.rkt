#lang sicp

; good-enough?とimprove演算は基本演算として使えると仮定版
(controller
 sqrt-loop
   (assign x (op read))
   (assign guess (constant 1.0))
 test-guess
   (test (op good-enough?) (reg x) (reg guess))
   (branch (label sqrt-loop-done))
   (assign guess (op improve) (reg x) (reg guess))
   (goto (label test-guess))
 sqrt-loop-done
   (perform (op print) (reg guess))
   (goto (label sqrt-loop)))
   

; 基本演算のみ
(controller
 sqrt-loop
   (assign x (op read))
   (assign guess (constant 1.0))
 test-guess
   ; (test (op good-enough?) (reg x) (reg guess))
   (assign t1 (op *) (reg guess) (reg guess))
   (assign t2 (op -) (reg t1) (reg x))
   (test (op >=) (reg t2) (constant 0))
   (branch (label test-abs))
   (assign t2 (op *) (reg t2) (constant -1)) ; t2が負の値の時は正にする
 test-abs
   (test (op <) (reg t2) (constant 0.001))
   (branch (label sqrt-loop-done))
   ; (assign guess (op improve) (reg x) (reg guess))
   (assign t3 (op /) (reg x) (reg guess))
   (assign t4 (op +) (reg t3) (reg guess))
   (assign guess (op /) (reg t4) (constant 2))
   (goto (label test-guess))
 sqrt-loop-done
   (perform (op print) (reg guess))
   (goto (label sqrt-loop))) 
 
 
