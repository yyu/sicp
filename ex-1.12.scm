(define (ptriangle row col)
  (cond ((> col row) 0)
	((= col 1) 1)
	((= col row) 1)
	(else (+
	       (ptriangle (- row 1) (- col 1))
	       (ptriangle (- row 1) col)))))
