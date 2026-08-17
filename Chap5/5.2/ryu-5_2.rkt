#lang sicp

(data-paths
 (registers
  ((name n))
  ((name product)
   (buttons ((name product<-1) (source (constant 1)))
            ((name product<-*) (source (operation *)))))
  ((name counter)
   (buttons ((name counter<-1) (source (constant 1)))
            ((name counter<-+) (source (operation +)))))))

(operations
 ((name *)
  (inputs (register product) (register counter)))
 ((name +)
  (inputs (register counter) (constant 1)))
 ((name >)
  (inputs (register counter) (register n))))

(controller
 (counter<-1)
 (product<-1)
 test-factrial ; ラベル
   (test (op >) (reg counter) (reg n)) ; テスト
   (branch (label factorial-done))
   (product<-*)
   (counter<-+)
   (goto (label test-factrial))
 factorial-done)
   
   
 
 