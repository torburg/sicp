#lang sicp

(#%require rackunit)

#|
#; 1.1
(define a 3)
(define b (+ a 1)) #; 4
(+ a b (* a b)) #; 19
(= a b) #; false
(if (and (> b a) (< b (* a b))) b #; 4
a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) #; 16
(+ 2 (if (> b a) b a)) #; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) #; 16


#; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))) #; - 37 / 150

#; 1.3
(define (max x y)
  (if (> x y) x
      y))

(define (min x y)
  (if (< x y) x
      y))

(define (sum x y z)
  (+ x y z))

(define (max-from-three x y z)
  (max (max x y) z))

(define (min-from-three x y z)
  (min (min x y) z))

(define (middle-from-three x y z)
  (- (sum x y z) (max-from-three x y z) (min-from-three x y z)))

|# (define (square x) (* x x)) #|

(define (sum-of-sqaures x y)
  (+ (square x) (square y)))

(define (sum-of-sqaures-from-three-numbers x y z)
  (sum-of-sqaures (max-from-three x y z) (middle-from-three x y z)))

(define (solution x y z)
  (sum-of-sqaures-from-three-numbers x y z))

1(check-equal? (solution 1 2 3) 13)
2(check-equal? (solution 4 2 3) 25)
3(check-equal? (solution 0 0 0) 0)
4(check-equal? (solution 1 0 1) 2)
5(check-equal? (solution 2 3 2) 13)

#; 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b -3 -3)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))


(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough guess x)
  (< (abs (- (square guess) x)) 0.0000001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
https://stackoverflow.com/questions/1171252/whats-the-explanation-for-exercise-1-6-in-sicp
#; 1.7

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     (* 0.0000001 guess)))

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))


(define (square-root a)
  (sqrt-iter 1.0 a))

(check-equal? (round (* 1000 (square-root 4.0))) 2000.0)
(check-equal? (round (* 1000 (square-root 100.0))) 10000.0)
(check-equal? (round (* 1000 (square-root 1000000.0))) 1000000.0)
(check-equal? (round (* 1000 (square-root 0.04))) 200.0)
(check-equal? (round (* 1000 (square-root 10000000000000000.0))) 100000000000.0)

; 1.8

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (improve guess x)
  (/
   (+
    (/ x (square guess))
    (* 2 guess))
   3))


(define (good-enough? guess x)
  (< (abs (- (cube guess) x))
     (* 0.0000001 guess)))

(define (cube-iter guess x)
  (if (good-enough? guess x)
          guess
          (cube-iter (improve guess x) x)))


(define (cube-root a)
  (cube-iter 1.0 a))


(check-equal? (round (* 1000 (cube-root 8.0))) 2000.0)
(check-equal? (round (* 1000 (cube-root 1000.0))) 10000.0)
(check-equal? (round (* 1000 (cube-root 1000000000.0))) 1000000.0)
(check-equal? (round (* 1000 (cube-root 0.008))) 200.0)

; 1.1.8
(define (square-root x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))

  (define (good-enough? guess)
    (< (abs (- (square guess) x))
       (* 0.0000001 guess)))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (define (square a) (* a a))
  
  (sqrt-iter 1.0))

(square-root 9)
; 1.9

; (define (+ a b)
;   (if (= a 0)
;       b
;       (inc (+ (dec a) b))))

; (+ 4 5)
; ((inc (+ 3 5)))
; ((inc (inc (+ 2 5))))
; ((inc (inc (inc (+ 1 5)))))
; ((inc (inc (inc (inc (+ 0 5))))))
; ((inc (inc (inc (inc 5)))))
; ((inc (inc (inc 6))))
; ((inc (inc 7)))
; ((inc 8))
; (9)


    
; (define (+ a b)
;   (if (= a 0)
;       b
;       (+ (dec a) (inc b))))

; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; (9)

; 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 0 2) ; (A 0 n) -> 2 * n
(A 1 3) ; (A 1 n) -> 2 ** n
(A 2 4) ; (A 2 n) -> 2 ** (A 2 (n - 1))

(A 2 5)
(A 1 (A 2 4))
(A 1 65536)))



(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))`
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 (A 0 (A 0 8192)))
(A 0 (A 0 16384))
(A 0 32768)
65536

(define (fib n)
  (cond ((= n 0) 0)
      ((= n 1) 1)
      (else (+ (fib (- n 1))
               (fib (- n 2))))))

(fib 5)

; (define (factorial n)
;   (if (= n 1) 1
;   (* n (factorial (- n 1)))))

; (define (factorial n)
;   (define (iter product counter)
;     (if (> counter n)
;         product
;         (iter (* counter product) (+ counter 1))))

;   (iter 1 1))

|#

; (define (f n)
;   (if (< n 3) n
;       (+ (f (- n 1))
;          (* 2 (f (- n 2)))
;          (* 3 (f (- n 3))))))

; (f 3)
; (+ (f 2) (* 2 (f 1)))
; (+ 2 (* 2 1))
; (+ 2 2)
; (4)

; (f 4)
; (+ (f 3) (* 2 (f 2) (* 3 (f 1)))) ; 
; (+ (+ (f 2) (* 2 (f 1)) (* 3 (f 0))) (* 2 2) (* 3 1))
; (+ (+ 2 (* 2 1) (* 3 0)) 4 3)
; (+ (+ 2 2 0) 4 3)
; (+ 4 4 3)
; (11)

; (f 3)
; (+ (f 2) (* 2 (f 1)) (* 3 (f 0)))

; (4)


; (define (f-iter n) 
;    (define (iter-impl a b c count) 
;       (if (= count 0) a 
;         (iter-impl b c (+ c (* 2 b) (* 3 a)) (- count 1)))) 
;    (iter-impl 0 1 2 n))

; (f-iter 4)
; (f-iter-impl 0) ; acc 0
; (+ (f-iter-impl 3) (* 2 (f-iter-impl 2)) (*3 f-iter-impl 1))
; (+ (+ (f-iter-impl ) ) (* 2 (f-iter-impl 2)) (*3 f-iter-impl 1))

; (check-equal? (f 4) 11)
; (check-equal? (f-iter 4) 11)
; (check-equal? (f 3) 4)
; (check-equal? (f-iter 1) 1)


; 1.12

; 11
; 21 22
; 31 32 33
; 41 42 43 44
; 51 52 53 54 55

; 1 (+ 51 52) (+ 52 53) (+ 53 54) (+ 54 55) 1
; 61 62 

;1
;1 1
;1 2 1
;1 3 3 1
;1 4 6 4 1

; (define (pascal-triangle row col)
;   (if (or (= row col) (= 1 col))
;     1
;     (+ (pascal-triangle (- row 1) (- col 1)) (pascal-triangle (- row 1) col))))

; (check-equal? (pascal-triangle 1 1) 1)
; (check-equal? (pascal-triangle 3 2) 2)
; (check-equal? (pascal-triangle 4 3) 3)
; (check-equal? (pascal-triangle 5 2) 4)
; (check-equal? (pascal-triangle 5 3) 6)

; 1.15
; (define (expt b n)
;   (expt-iter b n 1))
;   (define (expt-iter b counter product) (if (= counter 0)
;         product
;         (expt-iter b
;                   (- counter 1)
;                   (* b product))))


; (define (fast-expt b n) 
;   (cond ((= n 0) 1)
;         ((even? n) (square (fast-expt b (/ n 2))))
;         (else (* b (fast-expt b (- n 1))))))

; (define (even? n)
;   (= (remainder n 2) 0))


; slow remainder func
; (define (remainder n d)
;   (cond ((= n 0) 0)
;         ((< n 0) (+ n d))
;         (else (remainder (- n d) d))))

; faster remainder func
; (define (remainder n d)
;   (define (remainder-iter acc counter)
;     (cond ((= acc n) 0)
;           ((> acc n) (- n (- acc d)))
;           (else (remainder-iter (* d counter) (+ counter 1)))))

;   (remainder-iter d 1))

; (remainder 8 3)
; (remainder 5 3)
; (remainder 2 3)
; (remainder -1 3)

; (check-equal? (remainder 2 2) 0)
; (check-equal? (remainder 3 2) 1)
; (check-equal? (remainder 3 3) 0)
; (check-equal? (remainder 30 28) 2)

; 1.16
; (define (solution b n)
;   (define (fast-exp-iter b n a) 
;     (cond ((= n 0) a)
;           ((even? n) (fast-exp-iter (square b) (/ n 2) a))
;           (else (fast-exp-iter b (- n 1) (* a b)))))

;   (fast-exp-iter b n 1))

; (solution 2 5)
; (fast-exp-iter 2 5 1)
; (fast-exp-iter 2 4 2)
; (fast-exp-iter 2 2 4)
; (fast-exp-iter 2 1 16)
; (fast-exp-iter 2 0 32)

; (solution 2 10)
; (fast-exp-iter 2 10 1)
; (fast-exp-iter 2 5 1)
; (fast-exp-iter 2 4 2)
; (fast-exp-iter 2 2 4)
; (fast-exp-iter 2 1 16)
; (fast-exp-iter 2 0 32)

; (check-equal? (solution 2 5) 32)
; (check-equal? (solution 10 0) 1)
; (check-equal? (solution 3 20) (expt 3 20))
; (check-equal? (solution 2 10) (expt 2 10)) ; 1024
; (check-equal? (solution 0 5) 0)

; 1.17

; (define (double x) (* 2 x))

; (define (halve x)
;   (if (even? x) (/ x 2)))

; (define (* a b)
;   (if (= b 0)
;       0
;       (+ a (* a (- b 1)))))

; (define (fast-mul a b)
;   (cond ((= b 0) 0)
;         ((even? b) 
;           (double (fast-mul a (halve b))))
;         (else (+ a (fast-mul a (- b 1))))))



; (check-equal? (fast-mul 1 1) 1)
; (check-equal? (fast-mul 40 30) (* 40 30))
; (check-equal? (fast-mul 5 0) 0)
; (check-equal? (fast-mul 5 15) (* 5 15))

; 1.18

; (define (mul-iter a b)
;   (define (mul-iter-impl a b acc)
;     (cond ((= b 0) acc)
;         ((even? b) 
;           (mul-iter-impl (double a) (halve b) acc))
;         (else (mul-iter-impl a (- b 1) (+ acc a)))))

;   (mul-iter-impl a b 0))

; (mul-iter 5 5) ; 25
; (mul-iter-impl 5 5 0)
; (mul-iter-impl 5 4 5)
; (mul-iter-impl 5 2 20)
; (mul-iter-impl 5 1 50)
; (mul-iter-impl 5 0 55)

; (mul-iter 30 40) ; 1200
; (mul-iter-impl 30 20 0)
; (mul-iter-impl 30 10 0)
; (mul-iter-impl 30 5 0)
; (mul-iter-impl 30 4 30)
; (mul-iter-impl 30 2 60)
; (mul-iter-impl 30 1 120)
; (mul-iter-impl 30 0 150)

; (check-equal? (mul-iter 1 1) 1)
; (check-equal? (mul-iter 40 30) (* 40 30))
; (check-equal? (mul-iter 5 0) 0)
; (check-equal? (mul-iter 5 15) (* 5 15))


