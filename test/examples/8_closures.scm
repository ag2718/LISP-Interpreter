; Closures - functions capturing their environment
(define make-adder (lambda (n)
  (lambda (x) (+ x n))))

(define add5 (make-adder 5))
(add5 10)
(add5 100)

(define add10 (make-adder 10))
(add10 7)
(+ (add5 1) (add10 1))

