; Currying - transforming multi-arg to single-arg functions
(define curry-add (lambda (a)
  (lambda (b)
    (lambda (c) (+ a (+ b c))))))

(define add-from-1 (curry-add 1))
(define add-from-1-2 (add-from-1 2))
(add-from-1-2 3)
(((curry-add 10) 20) 30)

(define curry-mul (lambda (a)
  (lambda (b) (* a b))))

(define times-5 (curry-mul 5))
(times-5 10)
(times-5 7)
((curry-mul 3) 9)

