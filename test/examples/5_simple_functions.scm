; Simple function definitions and applications
(define square (lambda (x) (* x x)))
(square 5)
(square 12)
(define add (lambda (a b) (+ a b)))
(add 10 20)
(define triple (lambda (n) (* n 3)))
(triple (triple 2))
(define sum-of-squares (lambda (x y) (+ (square x) (square y))))
(sum-of-squares 3 4)

