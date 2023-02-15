#lang racket
(define (accumulate operator null list)
  (if (null? list)
      null
      (operator (car list)
                (accumulate operator null (cdr list)))))

; Problem 1
(define (contains? list object)
  (accumulate (lambda (first last) (+ first last) 10 10) 0 (list)))

(contains? (list 5 2 4 1) 5)