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
;Problems -> The algorithm doesnt allow for odd lengths
;  So, if you have a number that is 5 digits long and
;  square it you will get a number with 10 digits, however,
;  there is no middle of the number. 
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

; Problem 3
(define (is-prime num)
  (define (prime-calc num test)
    (if (> (sqrt num) test)
        (cond
          [(zero? (modulo num test)) #f]
          [(positive? (modulo num test)) (prime-calc num (+ test 1))])
        #t))
  (prime-calc num 2))
(define (next-mersenne n test)
  (cond
    [(boolean=? #t (test n))
     (cond
       [(boolean=? #t (test (sub1 (expt 2 n)))) n]
       [(boolean=? #f (test (sub1 (expt 2 n)))) (next-mersenne (add1 n) test)])]
    [(boolean=? #f (test n)) (next-mersenne (add1 n) test)]))

(printf "(next-mersenne 20 is-prime) = ~a\n" (next-mersenne 20 is-prime))
  