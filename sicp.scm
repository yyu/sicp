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

(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
	  ((= kinds-of-coins 2) 5)
	  ((= kinds-of-coins 3) 10)
	  ((= kinds-of-coins 4) 25)
	  ((= kinds-of-coins 5) 50)))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)					; c
	  ((or (< amount 0) (= kinds-of-coins 0)) 0)		; c'
	  (else (+ (cc amount
		       (- kinds-of-coins 1))
		   (cc (- amount
			  (first-denomination kinds-of-coins))	; c"
		       kinds-of-coins)))))
  (cc amount 5))

(display "(abs -3): ")
(display  (abs -3))
(newline)

(display "(abs 0): ")
(display  (abs 0))
(newline)

(display "(abs 3): ")
(display  (abs 3))
(newline)

(newline)

(display "(leet-abs -3): ")
(display  (leet-abs -3))
(newline)

(display "(leet-abs 0): ")
(display  (leet-abs 0))
(newline)

(display "(leet-abs 3): ")
(display  (leet-abs 3))
(newline)

(newline)

(display "(a-plus-abs-b 0 -3): ")
(display  (a-plus-abs-b 0 -3))
(newline)

(display "(a-plus-abs-b 0 0): ")
(display  (a-plus-abs-b 0 0))
(newline)

(display "(a-plus-abs-b 0 3): ")
(display  (a-plus-abs-b 0 3))
(newline)

(display "(a-plus-abs-b -2 -3): ")
(display  (a-plus-abs-b -2 -3))
(newline)

(display "(a-plus-abs-b -2 0): ")
(display  (a-plus-abs-b -2 0))
(newline)

(display "(a-plus-abs-b -2 3): ")
(display  (a-plus-abs-b -2 3))
(newline)

(display "(a-plus-abs-b 2 -3): ")
(display  (a-plus-abs-b 2 -3))
(newline)

(display "(a-plus-abs-b 2 0): ")
(display  (a-plus-abs-b 2 0))
(newline)

(display "(a-plus-abs-b 2 3): ")
(display  (a-plus-abs-b 2 3))
(newline)

(newline)

(display "(count-change 11): ")
(display  (count-change 11))
(newline)

(display "(count-change 0): ")
(display  (count-change 0))
(newline)

(display "(count-change 1): ")
(display  (count-change 1))
(newline)

(display "(count-change 4): ")
(display  (count-change 4))
(newline)

(display "(count-change 5): ")
(display  (count-change 5))
(newline)

(display "(count-change 6): ")
(display  (count-change 6))
(newline)

(display "(count-change 10): ")
(display  (count-change 10))
(newline)

(display "(count-change 100): ")
(display  (count-change 100))
(newline)

(newline)
