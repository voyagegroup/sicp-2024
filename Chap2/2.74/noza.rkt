#lang racket

; a.
; 事業所のファイルに事業所タグoffice-tagをつける
; また(office-tag file)で事業所タグを取得できるようにする
; そしてoffice-tagごとにnameとrecordを取得できるようにする

(define (get-record name file)
    (let ((tag (office-tag file)))
        (let ((name (get 'name tag))
              (record (get 'record tag)))
              (cond ((null? records) null)
                  ((eq? (name (car records)) name) (record (car records)))
                  (else (get-record name (cdr records)))))))


; b. 実装方針はaとほぼ同様。オフィスタグをつけた上で、(salary employee-record)を取得する手続きを作成する

; c.
(define (find-employee-record name files)
    (cond ((null? files) null)
          ((null? (get-record name (car files))) (find-employee-record name (cdr files)))
          (else (get-record name (car files)))))

; d. 事業所名（会社名）でのタグづけとそれぞれのキーで値を取得できるようなメソッドを実装する
