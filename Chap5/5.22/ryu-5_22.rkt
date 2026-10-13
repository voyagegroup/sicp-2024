#lang sicp

(define append-machine
  (make-machine
   '(x y val continue)
   (list (list 'null? null?)
         (list 'car car)
         (list 'cdr cdr)
         (list 'cons cons))
   '(start
     (assign continue (label append-done))
   append-loop
     (test (op null?) (reg x))
     (branch (label base-case))

     ; 再帰
     (save continue)
     (save x)
     (assign x (op cdr) (reg x))
     (assign continue (label after-append))
     (goto (label append-loop))

   base-case
     (assign val (reg y))
     (goto (reg continue))

   after-append
     ; 再帰から戻った後
     (restore x)
     (restore continue)
     (assign x (op car) (reg x))
     (assign val (op cons) (reg x) (reg val))
     (goto (reg continue))
   append-done)))

(set-register-contents! append-machine 'x '(1 2 3))
(set-register-contents! append-machine 'y '(4 5))

(start append-machine)

(get-register-contents append-machine 'val)
; (1 2 3 4 5)

;---- append! ---


(define append!-machine
  (make-machine
   '(x y current tmp)
   (list (list 'null? null?)
         (list 'cdr cdr)
         (list 'set-cdr! set-cdr!))

   '(start
     (assign current (reg x))

   append-loop
     ; curentの次が空か確認
     (assign tmp (op cdr) (reg current))
     (test (op null?) (reg tmp))
     (branch (label append-done))

     ; 次のペア
     (assign current (op cdr) (reg current))
     (goto (label append-loop))

   append-done
     ; 最後のペアのcdrをyへ
     (perform (op set-cdr!) (reg current) (reg y)))))


(define x (list 1 2 3))

(define y (list 4 5))

(set-register-contents! append!-machine 'x x)

(set-register-contents! append!-machine 'y y)

(start append!-machine)

(get-register-contents append!-machine 'x)

; (1 2 3 4 5)

x
; (1 2 3 4 5) こっちも書き換わっているjkj