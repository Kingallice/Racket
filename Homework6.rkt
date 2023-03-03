#lang plai

(define (operator-conv obj)
    (cond
        [(equal? obj '+) +]
        [(equal? obj '-) -]
        [(equal? obj '*) *]
        [(equal? obj '/) /]))

(define (build-subs params args tail)
  (if (null? params)
    tail
    (build-subs (rest params) (rest args) (aSub (first params) (first args) tail))))

(define-type FAE
  [ num (n number?)]
  [ binop (o procedure?) (lhs FAE?) (rhs FAE?)]
  [ id (name symbol?)]
  [ fun (args (listof symbol?)) (body FAE?)]
  [ app (fun-expr FAE?) (args (listof FAE?))])

(define-type FAE-Value
  [ numV (n number?)]
  [ closureV (params (listof symbol?))
             (body FAE?)
             (ds DefrdSub?)])

(define-type DefrdSub
  [ mtSub ]
  [ aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

;; lookup : symbol DefrdSub â†’ FAE-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [ mtSub () (error 'lookup "no binding for identifier" )]
    [ aSub (bound-name bound-value rest-ds)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-ds))]))

;; num+ : numV numV âˆ’â†’ numV
(define (numop o n1 n2)
  ( numV (o (numV-n n1) (numV-n n2))))

;; interp : FAE DefrdSub â†’ FAE-Value
(define (interp expr ds)
  (type-case FAE expr
    [ num (n) ( numV n)]
    [ binop (o l r) (numop o (interp l ds) (interp r ds))]
    [ id (v) (lookup v ds)]
    [ fun (bound-id bound-body)
          ( closureV bound-id bound-body ds)]
    [ app (fun-expr args)
          (local ([define fun-val (interp fun-expr ds)])
            (interp (closureV-body fun-val)
                    (build-subs (closureV-params fun-val)
                           (map (lambda (arg) (interp arg ds)) args)
                           (closureV-ds fun-val))
                      ))]))

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (first sexp)
; modify by parsing the following into a binop
       [(+ - * /) (binop (operator-conv (first sexp)) (parse (second sexp))
                 (parse (third sexp)))]
; modify by parsing the following into an app with multiple params
       [(with) (printf "-> ~a" (first (second sexp)))
        ;(with (first (second sexp))
         ;            (parse (second (second sexp)))
          ;           (parse (third sexp)))
               ]
; modify by parsing the following to handle multiple params
       [(fun) (fun 
                (second sexp)
                (parse (third sexp))
               )
              ]
       [else
        (cond ((list? (first sexp))  ;; this cond may not be needed
; modify by parsing the following to handle multiple params
               (app (parse (first sexp)) (map (lambda (x) (parse x)) (rest sexp)))))
        ]
       )]))

;; testing the interpreter on a function application (single param)
;(interp (parse '{{fun {x} {+ x x}} 5}) (mtSub) )

;; sample test cases
(printf "Sample Test Cases\n")
(test
 (interp (parse '5 ) [mtSub])
 (numV 5) )

(test
 (interp (parse '{{fun {x} {+ x x}} 5}) (mtSub))
 (numV 10))

 ;; Test Cases
 ; Part 1
 (printf "\nPart 1 Test Cases\n")
 (test
 (parse '{* 1 2})
 (binop * (num 1) (num 2)))

(test
 (interp (parse '{* 1 2}) (mtSub))
 (numV 2))

(test
 (parse '{* 2 {/ 3 4}})
 (binop * (num 2) (binop / (num 3) (num 4))))

(test
 (interp (parse '{* 2 {/ 3 4}}) (mtSub))
 (numV (/ 3 2)))

 ;; Part 2
 (printf "\nPart 2 Test Cases\n")
 (test
 (parse '{fun {x y z} {+ x x}})
 (fun '(x y z) (binop + (id 'x) (id 'x))))

(test
 (interp (parse '{fun {x y z} {+ x x}}) (mtSub))
 (closureV '(x y z) (binop + (id 'x) (id 'x)) (mtSub)))

;; Part 3
(printf "\nPart 3 Test Cases\n")
(test
 (interp (parse '{{fun {x y z} {* {+ x y} z}} 5 6 7}) (mtSub))
 (numV 77))

;; Part 4
(printf "\nPart 4 Test Cases\n")
(test
 (parse '{with {{x 5} {y 6} {z 7} } {* {+ x y} z} })
 (app (fun '(x y z) (binop * (binop + (id 'x) (id 'y)) (id 'z))) (list (num 5) (num 6) (num 7))))

(test
 (interp (parse '{with {{x 5} {y 6} {z 7} } {* {+ x y} z} }) (mtSub))
 (numV 77))

 ;; Part 5
 ;(printf "\nPart 5 Test Cases\n")