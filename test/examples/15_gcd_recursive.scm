; Greatest Common Divisor using Euclidean algorithm
(define mod (lambda (a b)
  (- a (* b (/ a b)))))
(mod 17 5)
(mod 100 7)
(define gcd (lambda (a b)
  (if (= b 0)
      a
      (gcd b (mod a b)))))
(gcd 48 18)
(gcd 100 25)
(gcd 17 13)
(gcd 144 89)

