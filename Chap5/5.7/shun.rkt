(define rec-expt-machine
  (make-machine
   '(b n val continue)
   (list (list '= =) (list '* *) (list '- -))
   '(controller
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

(set-register-contents! rec-expt-machine 'b 10)
(set-register-contents! rec-expt-machine 'n 3)


(start rec-expt-machine)

(get-register-contents rec-expt-machine 'val)
; 1,000


(define rep-expt-machine
  (make-machine
   '(b n counter product)
   (list (list '* *) (list '- -) (list '= =))
   '(controller
    (assign counter (reg n))
    (assign product (const 1))
 expt-iter
    (test (op =) (reg counter) (const 0))
    (branch (label expt-done))
    (assign counter (op -) (reg counter) (const 1))
    (assign product (op *) (reg b) (reg product))
    (goto (label expt-iter))
 expt-done)))

(set-register-contents! rep-expt-machine 'b 10)
(set-register-contents! rep-expt-machine 'n 4)

(start rep-expt-machine)

(get-register-contents rep-expt-machine 'product)
; 10,000
