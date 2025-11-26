; More complex recursive patterns
(define sum-to (lambda (n)
  (if (= n 0)
      0
      (+ n (sum-to (- n 1))))))
(sum-to 5)
(sum-to 10)
(sum-to 100)
(define power (lambda (base exp)
  (if (= exp 0)
      1
      (* base (power base (- exp 1))))))
(power 2 0)
(power 2 1)
(power 2 8)
(power 3 4)

