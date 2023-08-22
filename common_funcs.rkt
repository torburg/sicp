#lang sicp

(#%provide square)
(#%provide average)
(#%provide good-enough?)
(#%provide square-root)
(#%provide even?)
(#%provide next)
(#%provide identity)
(#%provide displayln)

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     (* 0.0000001 guess)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (square-root a)
  (sqrt-iter 1.0 a))


(define (next a)
    (if (= a 2) 3
      (+ a 2)))

(define (indentity x) x)

(define (displayln n)
  (newline)
  (display n))