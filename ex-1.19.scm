(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))      ; compute p'
                   (+ (* 2 p q)  (square q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
(define (square n)
  (* n n))

(display "(fib 0): ")
(display  (fib 0))
(newline)

(display "(fib 1): ")
(display  (fib 1))
(newline)

(display "(fib 2): ")
(display  (fib 2))
(newline)

(display "(fib 3): ")
(display  (fib 3))
(newline)

(display "(fib 5): ")
(display  (fib 5))
(newline)

(display "(fib 6): ")
(display  (fib 6))
(newline)

(display "(fib 7): ")
(display  (fib 7))
(newline)

