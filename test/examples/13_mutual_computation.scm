; Complex computations combining multiple features
(define square (lambda (x) (* x x)))
(define abs-diff (lambda (a b) (ABS (- a b))))
(abs-diff 10 3)
(abs-diff 3 10)
(define hypotenuse-sq (lambda (a b) (+ (square a) (square b))))
(hypotenuse-sq 3 4)
(hypotenuse-sq 5 12)
(define max-of-three (lambda (a b c) (MAX a b c)))
(max-of-three 5 9 3)
(max-of-three (square 2) (+ 1 1) (* 3 1))

