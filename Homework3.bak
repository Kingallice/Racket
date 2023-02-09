#lang racket
; Problem 1
; f1
(define (f1 a b)
  (let* ((x (* 2 a))
         (y (* 3 x)))
    (* x y)))

(printf "(f1 4 5) = ~a\n" (f1 4 5))

; f2
(define (f2 a b)
  (let ([x (* 2 a)])
    (let ([y (* 3 x)])
  (* x y))))

(printf "(f2 4 5) = ~a\n" (f2 4 5))
; f3
(define (f3 a b)
  ((lambda (x)
    ((lambda (y)
      (* x y))
    (* 3 x)))
  (* 2 a)))
(printf "(f3 4 5) = ~a\n" (f3 4 5))

; Problem 2
; bam
(newline)
;   Original
(define (bam_org a)
  ((lambda (x) (* x 5))
   (+ a 20)))
;   New
(define (bam_new a)
  (let ([x (+ a 20)])
        (* x 5)))
(printf "orginal (bam 3) = ~a
new (bam 3) = ~a\n" (bam_org 3) (bam_new 3))

; wam
(newline)
;   Original
(define (wam_org a b c)
  ((lambda (w x y) (* w x y))
   (+ a b) (* a b) (- a b)))
;   New
(define (wam_new a b c)
  (let ((w (+ a b)) (x (* a b)) (y (- a b)))
    (* w x y)))
(printf "orginal (wam 4 5 6) = ~a
new (wam 4 5 6) = ~a\n" (wam_org 4 5 6) (wam_new 4 5 6))

; zam
(newline)
;   Original
(define (zam_org a b c)
  ((lambda (x)
     ((lambda (y) (expt y c))
      (* x 3)))
   (+ a b)))
;   New
(define (zam_new a b c)
  (* a b c))
(printf "orginal (zam 5 6 7) = ~a
new (zam 5 6 7) = ~a\n" (zam_org 5 6 7) (zam_new 5 6 7))