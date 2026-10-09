(define bar 3)
bar
(define (foo x y) 1)
(foo 1 2)
(+ 1 2)
(+ (+ 1 2) 3)
(+ (+ 1 2) (+ 2 3) 4)
(define (add x y) (+ x y))
(add 1 2)
(add (add 1 2) 3)
(add (add 1 2) (add (add 2 3) 4))
(add (+ 1 2) 3)
(add (+ 1 2) 3)
(+ 2 3)
(let* ([a (+ 1 2)]) (add a a))
(let* ([a (add 1 2)]) (add a a))
(let* ([a (add 1 2)] [b (+ 3 4)]) (+ a b))

; (let* ([x (let* ([a 1]) a)]) a)  ; TODO make this use of `a` unbound
