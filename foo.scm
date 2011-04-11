(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (leet-abs x)
  (cond ((< x 0) (- x))
	((= x 0) 1337)
	(else x)))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
