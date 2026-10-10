```
(define append-machine
  (make-machine
   '(x y val continue)
   (list (list 'null? null?) (list 'cons cons) (list 'car car) (list 'cdr cdr))
   '(controller
     (assign continue (label append-done))
     append-loop
     (test (op null?) (reg x))
     (branch (label append-null))
     (save continue)
     (save x)
     (assign x (op cdr) (reg x))
     (assign continue (label after-append))
     (goto (label append-loop))
     after-append
     (restore x)
     (restore continue)
     (assign x (op car) (reg x))
     (assign val (op cons) (reg x) (reg val))
     (goto (reg continue))
     (branch (label append-null))
     (assign val (reg y))
     (goto (reg continue))
     append-null
     (assign val (reg y))
     (goto (reg continue))
     append-done)))
```

```
(set-register-contents! append-machine 'x (list 5))
(set-register-contents! append-machine 'y (list 6 7))
(start append-machine)
(get-register-contents append-machine 'val)
```

```
(define append!-machine
  (make-machine
   '(x y cdx lx continue)
   (list (list 'null? null?) (list 'set-cdr! set-cdr!) (list 'cdr cdr))
   '(controller
     (assign continue (label append!-done))
     (save x)
     last-pair
     (restore lx)
     (assign cdx (op cdr) (reg lx))
     (test (op null?) (reg cdx))
     (branch (label after-null))
     (assign lx (reg cdx))
     (save lx)
     (goto (label last-pair))
     after-null
     (assign lx (op set-cdr!) (reg lx) (reg y))
     (goto (reg continue))
     append!-done)))
```


```
(set-register-contents! append!-machine 'x (list 5))
(set-register-contents! append!-machine 'y (list 6 7))
(start append!-machine)
(get-register-contents append!-machine 'x)
; (5 6 7)
```