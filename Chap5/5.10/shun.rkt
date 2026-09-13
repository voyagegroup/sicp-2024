;constの接頭辞を不要にした。

(define (constant-exp? exp) (not (pair? exp)))

(define (constant-exp-value exp) exp)

; 構文の解釈の扱いのみを変えれば適用方法は変わらず、構文手続き以外の変更は不要に思う。