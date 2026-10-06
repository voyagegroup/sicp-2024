#lang sicp

; a. 再帰的count-leaves:
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(count-leaves (list 1 2 3 (cons 2 3)))
; → 5


(define count-leaves-machine-a
  (make-machine
   '(continue tree val tmp)
   (list (list 'null? null?) (list 'pair? pair?) (list 'car car) (list 'cdr cdr) (list '+ +))
   '(start
       (assign continue (label count-done))
   count-loop
     (test (op null?) (reg tree))
     (branch (label null-case))
     (test (op pair?) (reg tree)) ; pairじゃない場合は葉
     (branch (label pair-case)) ; pairの場合はpairのcaseへ
     (assign val (const 1))
     (goto (reg continue))
   null-case
     (assign val (const 0))
     (goto (reg continue))
   pair-case
     (save continue)
     (save tree)
     (assign tree (op car) (reg tree))
     (assign continue (label after-car))
     (goto (label count-loop))
   after-car
     (restore tree)
     (restore continue)
     (save continue) ; cdrの再帰でcontinueを上書きしてしまうので退避
     (save val)
     (assign tree (op cdr) (reg tree))
     (assign continue (label after-cdr))
     (goto (label count-loop))
   after-cdr
     (assign tmp (reg val)) ; valにcdrの結果が入っているので退避
     (restore val) ; carの結果をrestore
     (restore continue)
     (assign val (op +) (reg val) (reg tmp))
     (goto (reg continue))
   count-done)))

(set-register-contents! count-leaves-machine-a 'tree (list 1 2 3 (cons 2 3)))

(count-leaves-machine-a 'trace-on)

(start count-leaves-machine-a)
(get-register-contents count-leaves-machine-a 'val)

; → 5


; b. カウンタを陽に持つ再帰的count-leaves:
#|
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))


memo

(count-leaves (list (list 1 2 3) (list 5 8)))
(count-iter ((list 5 8))
            (count-iter (list 1 2 3) 0))

(count-iter (list 1 2 3) 0)
↓
(count-iter ((list 2 3))
            (count-iter  1 0))

(count-iter 1 0)
↓
(+ 0 1)

(count-iter ((list 2 3)) 1)
...
|#


(define count-leaves-machine-b
  (make-machine
   '(continue tree n)
   (list (list 'null? null?) (list 'pair? pair?) (list 'car car) (list 'cdr cdr) (list '+ +))
   '(start
       (assign continue (label count-done))
       (assign n (const 0))
   count-loop
     (test (op null?) (reg tree))
     (branch (label null-case))
     (test (op pair?) (reg tree)) ; pairじゃない場合は葉
     (branch (label pair-case)) ; pairの場合はpairのcaseへ
     (assign n (op +) (reg n) (const 1))
     (goto (reg continue))
   null-case
     (goto (reg continue))
     
   pair-case ; carの処理
     (save tree)
     (save continue)
     (assign tree (op car) (reg tree))
     (assign continue (label after-car))
     (goto (label count-loop))
     
   after-car ; cdr側の処理
     (restore continue)
     (restore tree)
     (save continue)
     (assign tree (op cdr) (reg tree))
     (assign continue (label after-cdr))
     (goto (label count-loop))

   after-cdr
     (restore continue)
     (goto (reg continue))
             
   count-done)))

(set-register-contents! count-leaves-machine-b 'tree (list 1 2 3 (cons 2 3)))
(count-leaves-machine-b 'trace-on)

(start count-leaves-machine-b)
(get-register-contents count-leaves-machine-b 'n)

; → 5