(define (f a b)
  (define (double n)
    (+ n n))
  (define (halve n)
    (/ n 2))
  (define (iter x a b)
    (cond ((= b 0) x)
	  ((even? b) (iter x (double a) (halve b)))
	  (else (iter (+ x a) a (+ -1 b)))))
  (iter 0 a b))

(display "(f 3 0): ")
(display  (f 3 0))
(newline)

(display "(f 3 1): ")
(display  (f 3 1))
(newline)

(display "(f 3 2): ")
(display  (f 3 2))
(newline)

(display "(f 3 3): ")
(display  (f 3 3))
(newline)

(display "(f 3 4): ")
(display  (f 3 4))
(newline)

(display "(f 3 5): ")
(display  (f 3 5))
(newline)
