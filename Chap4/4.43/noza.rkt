#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

; 父親 f のヨット名を返す（parker-y は変数）
(define (yacht-of father parker-y)
  (cond ((eq? father 'moore)   'lorna)
        ((eq? father 'hall)    'rosalind)
        ((eq? father 'hood)    'gabrielle)
        ((eq? father 'downing) 'melissa)
        ((eq? father 'parker)  parker-y)))

; 娘 d の父親を割り当てから返す
(define (father-of d moore-d downing-d hall-d hood-d parker-d)
  (cond ((eq? d moore-d)   'moore)
        ((eq? d downing-d) 'downing)
        ((eq? d hall-d)    'hall)
        ((eq? d hood-d)    'hood)
        ((eq? d parker-d)  'parker)))

; 共通の制約（Mary Ann の姓に関わらず成立するもの）
(define (solve moore-d-constraint)
  (let ((moore-d (amb 'mary-ann 'gabrielle 'lorna 'melissa 'rosalind)))
    (moore-d-constraint moore-d)
    (require (not (eq? moore-d 'lorna)))          ; Moore のヨット = Lorna
    (let ((downing-d (amb 'mary-ann 'gabrielle 'lorna 'melissa 'rosalind)))
      (require (not (member downing-d (list moore-d))))
      (require (not (eq? downing-d 'melissa)))    ; Downing のヨット = Melissa
      (let ((hall-d (amb 'mary-ann 'gabrielle 'lorna 'melissa 'rosalind)))
        (require (not (member hall-d (list moore-d downing-d))))
        (require (not (eq? hall-d 'rosalind)))    ; Hall のヨット = Rosalind
        (let ((hood-d (amb 'mary-ann 'gabrielle 'lorna 'melissa 'rosalind)))
          (require (not (member hood-d (list moore-d downing-d hall-d))))
          (require (not (eq? hood-d 'gabrielle))) ; Hood のヨット = Gabrielle
          (require (eq? hood-d 'melissa))         ; Hood の娘 = Melissa
          (let ((parker-d (amb 'mary-ann 'gabrielle 'lorna 'melissa 'rosalind)))
            (require (not (member parker-d (list moore-d downing-d hall-d hood-d))))
            (let ((parker-y (amb 'mary-ann 'gabrielle 'lorna 'melissa 'rosalind)))
              (require (not (eq? parker-y parker-d))) ; Parker のヨットは自分の娘の名前でない
              ; Gabrielle の父親は Parker の娘の名前のヨットを持っている
              (require (eq? (yacht-of (father-of 'gabrielle moore-d downing-d hall-d hood-d parker-d)
                                      parker-y)
                            parker-d))
              (list (list 'moore   moore-d)
                    (list 'downing downing-d)
                    (list 'hall    hall-d)
                    (list 'hood    hood-d)
                    (list 'parker  parker-d)
                    (list 'parker-yacht parker-y)))))))))

; --- Mary Ann の姓が Moore である場合 ---
(define (yacht-puzzle)
  (solve (lambda (moore-d) (require (eq? moore-d 'mary-ann)))))

; --- Mary Ann の姓が Moore だとわからない場合 ---
(define (yacht-puzzle-no-constraint)
  (solve (lambda (moore-d) 'no-constraint)))
