; a. number?やvariable?はタグとなるものが存在しないのでデータ主導プログラミングの振分けに吸収できない

#lang racket

; b. c.

(define (internal-deriv-package)
    (define (sum exp var)
        (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
    (define (product exp var)
        (make-product (deriv (multiplier exp) var)
                      (deriv (multiplicand exp) var)))
    (define (exponentiation exp var)
        (make-product
         (exponent exp)
         (make-product
          (make-exponentiation
           (base exp)
           (- (exponent exp) 1))
           (deriv (base exp) var))))
    (put '+ 'deriv sum)
    (put '* 'deriv product)
    (put '** 'deriv exponentiation))

; d.

; ((get (operator exp) 'deriv) (operands exp) var)
; 引数が式ではなく、operatorとoperandsで分けられるので、addendとmultiplicandのcadrがcdrになる
