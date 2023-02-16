#lang racket
(define (accumulate operator null list)
  (if (null? list)
      null
      (operator (car list)
                (accumulate operator null (cdr list)))))

; Problem 1
; Conditional
(define (contains-cond? list object)
  (if (empty? list)
      #f
     (if (= object (car list)) #t
         (contains-cond? (rest list) object))))
(printf "Conditional - (contains? (list 1 8 23 2 12 8 9) 3) = ~a\n" (contains-cond? (list 1 8 23 2 12 8 9) 3))
(printf "Conditional - (contains? (list 1 8 23 2 12 8 9) 8) = ~a\n" (contains-cond? (list 1 8 23 2 12 8 9) 8))

; No Conditional
(define (contains? list object)
  (accumulate (lambda (x y) (or x y))
              #f (map (lambda (x) (= object x)) list)))

(printf "No Conditional - (contains? (list 1 8 23 2 12 8 9) 3) = ~a\n" (contains? (list 1 8 23 2 12 8 9) 3))
(printf "No Conditional - (contains? (list 1 8 23 2 12 8 9) 8) = ~a\n" (contains? (list 1 8 23 2 12 8 9) 8))