#lang racket
; Problem 1
; A
(define (reverse-digits n)
  (if (= n 0)
      ""
  (string-append (~a (modulo n 10))(reverse-digits (floor (/ n 10))))))

(reverse-digits 37821)