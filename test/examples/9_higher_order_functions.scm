; Higher-order functions - functions that take functions as arguments
(define apply-twice (lambda (f x)
  (f (f x))))

(define double (lambda (n) (* n 2)))
(apply-twice double 3)

(define increment (lambda (n) (+ n 1)))
(apply-twice increment 10)
(apply-twice (lambda (x) (* x x)) 2)

(define compose (lambda (f g)
  (lambda (x) (f (g x)))))

(define double-then-inc (compose increment double))
(double-then-inc 5)

(define inc-then-double (compose double increment))
(inc-then-double 5)

