#lang racket

(require amb)

(define (require p)
  (if (not p) (amb) 'done))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

; 問題 4.39
;
; require の順序は解に影響するか？
;   → しない。全制約を満たす組み合わせは順序によらず同じ。
;
; 解を見出す時間に影響するか？
;   → する。let で変数が全て束縛されてから require が評価されるため、
;     常に 5^5 = 3125 通りが生成される。
;     しかし各組み合わせに対するチェックコストが順序で変わる。
;     - 単一変数の制約 (baker ≠ 5 など) は O(1)
;     - distinct? は O(n²) でコストが高い
;     → 安い制約を先に並べ、distinct? を最後にすると
;       早期に失敗して distinct? の呼び出し回数が減る。

; 元の順序（distinct? が最初）
(define (multiple-dwelling-original)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))
    (require (distinct? (list baker cooper fletcher miller smith))) ; コスト高
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
          (list 'smith    smith))))

; 改善版（O(1) の制約を先に、distinct? を最後に）
(define (multiple-dwelling-faster)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (distinct? (list baker cooper fletcher miller smith))) ; 最後
    (list (list 'baker    baker)
          (list 'cooper   cooper)
          (list 'fletcher fletcher)
          (list 'miller   miller)
          (list 'smith    smith))))

; 解の確認（両者とも同じ解が得られる）
(stream->list (in-amb (multiple-dwelling-faster)))

; '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
