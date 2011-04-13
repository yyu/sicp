(define (* a b)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (double n)
    (+ n n))
  (define (halve n)
    (/ n 2))
  (cond ((or (= a 0) (= b 0)) 0)
	((even? b) (* (double a) (halve b)))
	(else (+ (* a (- b 1)) a))))

(display "(* 0 0): ")
(display  (* 0 0))
(newline)

(display "(* 3 0): ")
(display  (* 3 0))
(newline)

(display "(* 0 3): ")
(display  (* 0 3))
(newline)

(display "(* 1 5): ")
(display  (* 1 5))
(newline)

(display "(* 3 1): ")
(display  (* 3 1))
(newline)

(display "(* 3 5): ")
(display  (* 3 5))
(newline)

(display "(* 3 8): ")
(display  (* 3 8))
(newline)

(display "(* 2 5): ")
(display  (* 2 5))
(newline)

(display "(* 8 10): ")
(display  (* 8 10))
(newline)
