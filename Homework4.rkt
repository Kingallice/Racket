#lang racket
(define (accumulate operator null lst)
  (if (null? lst)
      null
      (operator (car lst)
                (accumulate operator null (cdr lst)))))

; Problem 1
; a
(printf "Problem 1\na.\n")
; Conditional
(define (contains-cond? lst object)
  (if (empty? lst)
      #f
     (if (equal? object (car lst)) #t
         (contains-cond? (rest lst) object))))
(printf "Conditional - (contains? (list 1 8 23 2 12 8 9) 3) = ~a\n" (contains-cond? (list 1 8 23 2 12 8 9) 3))
(printf "Conditional - (contains? (list 1 8 23 2 12 8 9) 8) = ~a\n" (contains-cond? (list 1 8 23 2 12 8 9) 8))

; No Conditional
(define (contains-acc? lst object)
  (accumulate (lambda (x y) (or x y))
              #f (map (lambda (x) (equal? object x)) lst)))

(printf "\nNo Conditional - (contains? (list 1 8 23 2 12 8 9) 3) = ~a\n" (contains-acc? (list 1 8 23 2 12 8 9) 3))
(printf "No Conditional - (contains? (list 1 8 23 2 12 8 9) 8) = ~a\n" (contains-acc? (list 1 8 23 2 12 8 9) 8))

; b
; Recursive
(printf "\nb.\n")
(define (remove-rec object lst)
  (if (> (length lst) 0)
      (if (equal? object (first lst)) (remove-rec object (rest lst))
          (append (list (first lst)) (remove-rec object (rest lst))))
      lst))

(printf "Recursive - (remove 1 (list 1 4 2 1 6 3 5)) = ~a\n" (remove-rec 1 (list 1 4 2 1 6 3 5)))
(printf "Recursive - (remove 5 (list 1 4 2 1 6 3 5)) = ~a\n" (remove-rec 5 (list 1 4 2 1 6 3 5)))

; Accumulate
(define (remove-acc object lst)
  (accumulate (lambda (x rst)
                (if (equal? object x)
                    rst
                    (cons x rst))) null lst))
    
(printf "\nAccumulate - (remove 5 (list 1 4 2 1 6 3 5)) = ~a\n" (remove-acc 1 (list 1 4 2 1 6 3 5)))
(printf "Accumulate - (remove 5 (list 1 4 2 1 6 3 5)) = ~a\n" (remove-acc 5 (list 1 4 2 1 6 3 5)))

; Problem 2
(printf "\nProblem 2\n")
;; 1 + (2 / (3 * 5 + 1)) + (-4)
(define expr
   (list +
         1
         (list /
               2
               (list +
                     (list * 3 5)
                     1))
         (list - 0 4)))
expr
;Value: (+ 1 (/ 2 (+ (* 3 5) 1)) (- 0 4))
; a
(define (operator? item)
  (contains-acc? (list + - * /) item))
; b
(define (null-val operator)
  (if (contains-acc? (list + -) operator) 0 1))
; c
(newline)
(define (expression? expr)
  (if (> (length expr) 2)
      (if (operator? (first expr))
          (if (accumulate
               (lambda (x y) (or x y)) #f
               (map (lambda (x) (operator? x)) (rest expr)))
              #f
              (accumulate
               (lambda (x y) (or x y)) #f
               (map (lambda (x)
                     (cond
                       [(number? x) #t]
                       [(list? x) (expression? x)])) (rest expr))))
          #f)
      #f))

; d
(define (count-operators expr)
  (if (null? expr)
      0
      (cond
        [(operator? (first expr)) (+ (count-operators (rest expr)) 1)]
        [(number? (first expr)) (+ (count-operators (rest expr)) 0)]
        [(list? (first expr)) (+ (count-operators (first expr)) (count-operators (rest expr)))])))

(define (count-primitive-operands expr)
    (if (null? expr)
      0
      (cond
        [(operator? (first expr)) (+ (count-primitive-operands (rest expr)) 0)]
        [(number? (first expr)) (+ (count-primitive-operands (rest expr)) 1)]
        [(list? (first expr)) (+ (count-primitive-operands (first expr)) (count-primitive-operands (rest expr)))])))

; e
(define (evaluate expr)
  (if (expression? expr)
      (accumulate
       (first expr) (null-val (first expr))
       (map (lambda (x)
              (cond
                [(list? x) (evaluate x)]
                [(number? x) x])) (rest expr)))
      "ERROR"))

(printf "(operator? (car expr)) \t\t<#t> = ~a\n" (operator? (car expr)))
;Value: #t 
(printf "(number? (cadr expr)) \t\t<#t> = ~a\n" (number? (cadr expr)))
;Value: #t
(printf "(expression expr) \t\t<#t> = ~a\n" (expression? expr))
;Value: #t 
(printf "(expression (list 1 + 2)) \t<#f> = ~a\n" (expression? (list 1 + 2))) 
;Value: #f
(printf "(expression (list + 1)) \t<#f> = ~a\n" (expression? (list + 1)))
;Value: #f 
(printf "(count-operators expr) \t\t<5> = ~a\n" (count-operators expr))
;Value: 5
(printf "(count-primitive-operands expr) <7> = ~a\n" (count-primitive-operands expr))
;Value: 7 
(printf "(evaluate expr) \t\t<-23/8> = ~a\n" (evaluate expr))
;Value: -23/8
