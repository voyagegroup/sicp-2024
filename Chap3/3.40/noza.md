# 解答

> (define x 10)
>
> (define s (make-serializer))
>
> (parallel-execute (s (lambda () (set! x (* x x))))
>                   (s (lambda () (set! x (+ x 1)))))
> ではxの可能な値として101と121の二つしか生じない. P1とP2は混ざり合うことは出来ないので, 他の可能性は排除される.

と同じ形式なので、可能な値は 2 つ
x: (10^2)^3
x: (10^3)^2
