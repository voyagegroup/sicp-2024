#lang sicp

(assert! (rule (append-to-form () ?y ?y)))

(assert!
 (rule (append-to-form (?u . ?v) ?y (?u . ?z))
       (append-to-form ?v ?y ?z)))

(assert! (rule (reverse () ())))

(assert!
 (rule (reverse (?first . ?rest) ?result)
       (and
        (reverse ?rest ?reversed-rest)
        (append-to-form ?reversed-rest (?first) ?result))))


;;; Query input:
(reverse (1 2 3) ?x)

;;; Query results:
(reverse (1 2 3) (3 2 1))

;;; Query input:
(reverse ?x (1 2 3))

;;; Query results:

; 固まった

; (reverse ?rest ?reversed-rest) を先に解こうとして、?rest も ?reversed-rest も未束縛でかえしている