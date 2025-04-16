#lang racket

;; key が数値で順序づけられているに信義で構造化されている場合の lookup

(define (lookup given-key set-of-records)
  (let ((en (entry set-of-records)))
    (cond ((null? set-of-records) false)
          ((= given-key (key en)) en)
          ((< given-key (key en)) (lookup given-key (left-branch set-of-records)))
          (else (lookup given-key (right-branch set-of-records))))))

;; ただし、データは (key value) のペア
;; key は (key value) のペアの key を取得する関数
;; set-of-records は ((key value) left-branch right-branch) のリストとする
