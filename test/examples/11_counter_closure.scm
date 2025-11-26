; Counter using closures (demonstrating captured state)
(define make-counter (lambda (start)
  (lambda (step) (+ start step))))

(define counter (make-counter 100))
(counter 1)
(counter 5)
(counter 10)

(define another (make-counter 0))
(another 1)
(another 50)

