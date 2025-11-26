; Nested function definitions and local bindings in lambda bodies
(define outer (lambda (x)
  (let ((inner-val (* x 2)))
    (+ inner-val 1))))
(outer 5)
(outer 10)

(define with-helper (lambda (n)
  (let ((step1 (+ n 10)))
    (let ((step2 (* step1 2)))
      (- step2 5)))))
(with-helper 5)
(with-helper 0)

