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
; a

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

(define (operator? item)
  (contains-acc? (list + - * /) item))

(define (null-val operator)
  (if (contains-acc? (list + -) operator) 0 1))

(define (expression? expr)
  (if (length expr)
      (if (operator? (car expr))
          #t
          #f)
      #f))
        


;Value: (+ 1 (/ 2 (+ (* 3 5) 1)) (- 0 4))

(printf "(operator? (car expr)) <#t> = ~a\n" (operator? (car expr)))
;Value: #t 
(printf "(number? (cadr expr)) <#t> = ~a\n" (number? (cadr expr)))
;Value: #t
(expression? expr) 
;Value: #t 
(expression? (list 1 + 2)) 
;Value: #f
(expression? (list + 1)) 
;Value: #f 
;(count-operators expr) 
;Value: 5
;(count-primitive-operands expr) 
;Value: 7 
;(evaluate expr) 
;Value: -23/8
