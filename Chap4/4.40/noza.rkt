#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

; 問題 4.40
;
; distinct? 適用前後の組み合わせ数:
;   適用前: 5^5 = 3125 通り
;   適用後: 5! = 120 通り
;
; 改善策: let を入れ子にして「前の人と同じ階にはならない」制約だけを
; 各変数の束縛直後に課す。パズル固有の制約は従来通り require で並べる。

(define (multiple-dwelling-efficient)
  (let ((baker (amb 1 2 3 4 5)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (member cooper (list baker))))
      (let ((fletcher (amb 1 2 3 4 5)))
        (require (not (member fletcher (list baker cooper))))
        (let ((miller (amb 1 2 3 4 5)))
          (require (not (member miller (list baker cooper fletcher))))
          (let ((smith (amb 1 2 3 4 5)))
            (require (not (member smith (list baker cooper fletcher miller))))
            (require (not (= baker 5)))
            (require (not (= cooper 1)))
            (require (not (= fletcher 5)))
            (require (not (= fletcher 1)))
            (require (> miller cooper))
            (require (not (= (abs (- smith fletcher)) 1)))
            (require (not (= (abs (- fletcher cooper)) 1)))
            (list (list 'baker    baker)
                  (list 'cooper   cooper)
                  (list 'fletcher fletcher)
                  (list 'miller   miller)
                  (list 'smith    smith))))))))

(stream->list (in-amb (multiple-dwelling-efficient)))

; '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
