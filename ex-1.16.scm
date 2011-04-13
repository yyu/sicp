;; solution from http://community.schemewiki.org/?sicp-ex-1.16
(define (fast-expt b n) 
  (define (square x) (* x x))
  (define (iter a b n) 
    (cond ((= n 0) a) 
	  ((even? n) (iter a (square b) (/ n 2))) 
	  (else (iter (* a b) b (- n 1))))) 
  (iter 1 b n))

;; my solution
(define (expt-iter b n)
  (define (iter a x k m)
    (if (= m 0)
	a
	(if (> k m)
	    (iter (* a b)      b       1  (- m 1))
	    (iter (* a x) (* x x) (* k 2) (- m k)))))
  (iter 1 b 1 n))

;; test
(define expt expt-iter)			; test my solution
;(define expt fast-expt)		; test schemewiki solution

(display "(expt 2^0): ")
(display  (expt 2 0))
(newline)

(display "(expt 2^1): ")
(display  (expt 2 1))
(newline)

(display "(expt 2^2): ")
(display  (expt 2 2))
(newline)

(display "(expt 2^9): ")
(display  (expt 2 9))
(newline)

(display "(expt 2^10): ")
(display  (expt 2 10))
(newline)
