; Let bindings - creates local variables scoped to the body
; Syntax: (let ((var1 val1) (var2 val2) ...) body...)
; Returns the value of the last expression in the body

; Simple let with one binding
(let ((x 10)) x)

; Let with multiple bindings
(let ((x 10) (y 20)) (+ x y))

; Nested arithmetic in let body
(let ((a 5) (b 3)) (* a (+ b 2)))

; Let with conditional in body
(let ((x 15)) (if (> x 10) 100 0))

; Nested lets
(let ((x 10))
  (let ((y 20))
    (+ x y)))

; Let where binding uses outer scope
(define z 50)
(let ((a 10)) (+ a z))

; Let with computed initial values
(let ((x (+ 2 3)) (y (* 4 5))) (- y x))

; Multiple expressions in body - returns last value
(let ((x 5))
  (+ x 1)
  (+ x 2)
  (+ x 3))

