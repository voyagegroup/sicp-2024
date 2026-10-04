## a

```
(define rec-count-leaves-machine
  (make-machine
   '(tree val continue n)
   (list (list 'null? null?) (list 'pair? pair?) (list '+ +) (list 'car car) (list 'cdr cdr))
   '(controller
     (assign continue (label count-done))
     count-loop
     (test (op null?) (reg tree))
     (branch (label count-null))
     (test (op pair?) (reg tree))
     (branch (label count-pair))
     (assign val (const 1));葉
     (goto (reg continue))
     count-null
     (assign val (const 0))
     (goto (reg continue))
     count-pair
     (save continue)
     (save tree)
     (assign continue (label after-car))
     (assign tree (op car) (reg tree))
     (goto (label count-loop))
     after-car
     (restore tree)
     (save val)
     (assign tree (op cdr) (reg tree))
     (assign continue (label after-cdr))
     (goto (label count-loop))
     after-cdr
     (restore n) ;n=carの結果
     (restore continue)
     (assign val
             (op +)
             (reg n)
             (reg val))
     (goto (reg continue))
     count-done)))
```

## b

```
(define itr-count-leaves-machine
  (make-machine
   '(tree n continue)
   (list (list 'null? null?) (list 'pair? pair?) (list '+ +) (list 'car car) (list 'cdr cdr))
   '(controller
     (assign continue (label itr-done))
     (assign n (const 0))
     count-itr
     (test (op null?) (reg tree))
     (branch (label count-null))
     (test (op pair?) (reg tree))
     (branch (label count-pair))
     (assign n (op +) (reg n) (const 1))
     (goto (reg continue))
     count-null
     (goto (reg continue))
     count-pair
     (save continue)
     (save tree)
     (assign continue (label after-car))
     (assign tree (op car) (reg tree))
     (goto (label count-itr))
     after-car
     (restore tree)
     (restore continue)
     (assign tree (op cdr) (reg tree))
     (goto (label count-itr))
     itr-done)))
```

## 検証用

```
(set-register-contents! itr-count-leaves-machine 'tree '((1 2) (3 (4 5))))
(start itr-count-leaves-machine)
(get-register-contents itr-count-leaves-machine 'n)

; 5
```