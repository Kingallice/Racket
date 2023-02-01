#lang racket
; Problem 1
(display "1. ")(newline)
; Miles-to-KM
(define (miles-to-km miles)
  (* miles 1.609)
)

(display "(miles-to-km 10) = ")(miles-to-km 10)(newline)

; KM-to-Miles
(define (km-to-miles km)
  (/ km 1.609)
)

(display "(km-to-miles 16.09) = ")(km-to-miles 16.09)(newline)

(display "2. ")(newline)
(define (foo x y) 
   (+ x y (- x)))

(display "A. ")(foo (- 2) 4)

(define (a b c d)
   (e b c d))

(define f +)

(define e f)

(display "B. ")(a (+ 3 2) (* 4 5) 3)

(define (a1 b c d)
   ((e1 (f1 b)) c d))

(define (e1 a) 
   a)

(define (f1 a)
   (if (positive? a) 
       +
       -))

(display "C. ")(a1 1 (+ 2 3) 4)
(newline)

; Problem 3
(display "3. ")

(define (baz n)
  (define (bar a b)
    (define (foo a)
   (* a (+ a 2)))
   (if (= a 14)
       b
       (bar (+ a 1) (+ (foo a) b))))
   (bar 3 n))

(baz 8)(newline)

; Problem 4
(display "4.")(newline)
; Function
(define (sum-range from to)
  (cond
    [(zero? (- from to)) from]
    [(positive? (- from to))(+ from (sum-range (- from 1) to))]
    [(negative? (- from to))(+ from (sum-range (+ from 1) to))]))
; Testing
(display "(sum-range 7 7) = ")(sum-range 7 7)
(display "(sum-range 4 18) = ")(sum-range 4 18)
(display "(sum-range 18 4) = ")(sum-range 18 4)