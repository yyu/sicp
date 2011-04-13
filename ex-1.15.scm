(define pi 3.14159265)

(define (sine angle)
  (define (cube x) (* x x x))
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(display "(sine 3): ")
(display  (sine 3))
(newline)

(display "(sine 3.14): ")
(display  (sine 3.14))
(newline)

(display "(sine pi): ")
(display  (sine pi))
(newline)

(display "(sine (/ pi 2)): ")
(display  (sine (/ pi 2)))
(newline)

(display "(sine (* pi 2)): ")
(display  (sine (* pi 2)))
(newline)

(display "(sine (* 3 (/ pi 2))): ")
(display  (sine (* 3 (/ pi 2))))
(newline)

(newline)
