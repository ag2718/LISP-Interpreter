; Recursive Fibonacci function
(define fib (lambda (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))
(fib 0)
(fib 1)
(fib 2)
(fib 5)
(fib 10)

