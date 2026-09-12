
(define here-machine
  (make-machine
   '(a)
   '()
   '(start
     (goto (label here))
     here
     (assign a (const 3))
     (goto (label there))
     here
     (assign a (const 4))
     (goto (label there))
     there)))

(start here-machine)

(get-register-contents here-machine 'a)
; 3


(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (if (assoc next-inst labels); labelsの中にnext-instがあると#t
                                  (error "Duplicated label" next-inst)
                                  (receive insts
                                       (cons (make-label-entry next-inst
                                                               insts)
                                             labels)))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))

; Duplicated label here