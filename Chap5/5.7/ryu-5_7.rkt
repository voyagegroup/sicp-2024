#lang sicp

; a. 再帰的べき乗:
(define expt-machine-a
  (make-machine
   '(b n val continue)
   (list
    (list '* *)
    (list '- -)
    (list '= =))
   '(
     (assign continue (label expt-done))
     expt-loop
       (test (op =) (reg n) (const 0))
       (branch (label base-case))
       (save continue)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-expt))
       (goto (label expt-loop))
     after-expt
       (restore continue)
       (assign val (op *) (reg b) (reg val))
       (goto (reg continue))
     base-case
       (assign val (const 1))
       (goto (reg continue))
     expt-done)))
   
(set-register-contents! expt-machine-a 'b 2)
(set-register-contents! expt-machine-a 'n 3)
(start expt-machine-a)
(get-register-contents expt-machine-a 'val)
; 8


; b. 反復的べき乗
(define expt-machine-b
  (make-machine
   '(b n val counter product)
   (list
    (list '* *)
    (list '- -)
    (list '= =))
   '(
     (assign counter (reg n))
     (assign product (const 1))
     expt-loop
       (test (op =) (reg counter) (const 0))
       (branch (label expt-done))
       (assign counter (op -) (reg counter) (const 1))
       (assign product (op *) (reg b) (reg product))
       (goto (label expt-loop))
     expt-done
     )))

(set-register-contents! expt-machine-b 'b 2)
(set-register-contents! expt-machine-b 'n 3)
(start expt-machine-b)
(get-register-contents expt-machine-b 'product)
; 8