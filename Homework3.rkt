#lang racket
; Problem 1
; f1
(printf "Problem 1\n")
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
(newline)(printf "Problem 2\n")
; bam
;   Original
(define (bam_org a)
  ((lambda (x) (* x 5))
   (+ a 20)))
;   New
(define (bam_new a)
  (let ([x (+ a 20)])
        (* x 5)))
(printf "a.\norginal (bam 3) = ~a
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
(printf "b.\norginal (wam 4 5 6) = ~a
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
  (let ([x (+ a b)])
        (let ([y (* x 3)])
          (expt y c))))
(printf "c.\norginal (zam 5 6 7) = ~a
new (zam 5 6 7) = ~a\n" (zam_org 5 6 7) (zam_new 5 6 7))

; Problem 3
(newline)(printf "Problem 3\n")
; a
(define (wacky_org x y)
  (define (tacky x z)
    (define (snacky y z)
      (+ x y z))
    (snacky z x))
  (tacky y x))

(define (wacky_new x1 y1)
  (define (tacky x2 z1)
    (define (snacky y2 z2)
      (+ x2 y2 z2))
    (snacky z1 x2))
  (tacky y1 x1))

(printf "a.\noriginal (wacky 3 4) = ~a
new (wacky 3 4) = ~a\n" (wacky_org 3 4) (wacky_new 3 4))
; b
(newline)
(define (f1_org a b)
  (define (g x y) (* x y))
  (define (h m n) (- m n))
  (+ (g a b) (h b a)))

(define (f1_new a b)
  (define (g a b) (* a b))
  (define (h a b) (- a b))
  (+ (g a b) (h b a)))

(printf "b.\norginal (f1 5 6) = ~a
new (f1 5 6) = ~a\n" (f1_org 5 6) (f1_new 5 6))
; c
(newline)
(define (f2_org a b)
  (define (g x y) (+ a x y))
  (define (h m n) (* b m n))
  (+ (g a b) (h b a)))

(define (f2_new a b)
  (define (g b c) (+ a b c))
  (define (h a c) (* b a c))
  (+ (g a b) (h b a)))

(printf "c.\norginal (f2 3 4) = ~a
new (f2 3 4) = ~a\n" (f2_org 3 4) (f2_new 3 4))

; d
(newline)
(define (f3_org a)
  (define (g1 b)
    (define (h1 c) (expt c a))
    (define (h2) (- a b))
    (+ (h1 b) (h2)))
  (+ a (g1 a)))

(define (f3_new x)
  (define (foo y)
    (define (bar y) (expt y x))
    (define (foo) (- x y))
    (+ (bar y) (foo)))
  (+ x (foo x)))

(printf "d.\norginal (f3 3) = ~a
new (f3 3) = ~a\n" (f3_org 3) (f3_new 3))

; Problem 4
(newline)(printf "Problem 4\n")
; a
;   Original
(define (poly-3 x a0 a1 a2 a3)
   (+ a0 (* a1 x) (* a2 x x) (* a3 x x x)))
;   New
(define (make-poly-3 a0 a1 a2 a3)
  (lambda (x) (+ a0 (* a1 x) (* a2 x x) (* a3 x x x))))
(define my-poly-3
  (make-poly-3 1 2 3 4))

(printf "a.\n(poly-3 2 1 2 3 4) = ~a
(my-poly-3 2) = ~a\n" (poly-3 2 1 2 3 4) (my-poly-3 2))

;b
(newline)
(define (sum-poly-3-range from to)
  (if (> from to)
      0
      (+ (sum-poly-3-range (add1 from) to) (my-poly-3 from))))

 (printf "b. (sum-poly-3-range 1 50) = ~a" (sum-poly-3-range 1 50))