(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

(define (ff n)
  (define (f-iter fn fn-1 fn-2 counter)
    (if (> counter n)
	fn
	(f-iter (+ fn
		   (* 2 fn-1)
		   (* 3 fn-2))
		fn
		fn-1
		(+ counter 1))))
  (if (< n 3)
      n
      (f-iter 2 1 0 3)))
