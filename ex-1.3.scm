(define (sum-of-square x y)
  (+ (* x x) (* y y)))
;Value: sum-of-square

(sum-of-square 2 3)
;Value: 13

(define (f x y z)
  (cond ((and (< x y) (< x z)) (sum-of-square y z))
	((< y z) (sum-of-square x z))
	(else (sum-of-square x y))))
;Value: f

(f 1 2 3)
;Value: 13

(f 1 3 2)
;Value: 13

(f 3 1 2)
;Value: 13

(f 3 2 1)
;Value: 13

(f 2 1 3)
;Value: 13

(f 2 3 1)
;Value: 13
