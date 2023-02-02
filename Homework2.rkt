#lang racket
; Problem 1
; A
(define (reverse-digits n)
  (define (reverse n)
    (if (= n 0)
        ""
        (string-append (~a (modulo n 10))(reverse (floor (/ n 10))))))
  (string->number (reverse n)))

(printf "(reverse-digits 37821) = ~a\n" (reverse-digits 37821))

;B
(define (same-digits a b)
  (equal?
   (sort
    (string->list
     (number->string a)) char<?)
   (sort
    (string->list
     (number->string b)) char<?)))

(printf "(same-digits 7361 7136) = ~a\n" (same-digits 7361 7136))

;Problem 2
(define (middle-square prev len)
  (define (get-list number)
    (string->list (number->string (expt number 2))))
  
  (define (middle-numbers list len cnt)
    (cond
      [(zero? cnt)
       (cond
         [(positive? len) "" (string-append (~a (first list))(~a (middle-numbers (rest list) (- len 1) 0)))]
         [(zero? len) ""])]
      [(positive? cnt) (middle-numbers (rest list) len (- cnt 1))]))

  (string->number
   (middle-numbers (get-list prev) len (/ (- (length (get-list prev)) len) 2))))

(printf "(middle-square 5772156649 10) = ~a\n" (middle-square 5772156649 10))
(printf "(middle-square 46283 6) = ~a\n" (middle-square 46283 6))
  