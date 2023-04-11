#lang plai

(define-type FAE
  [ num (n number?)]
  [ add (lhs FAE?) (rhs FAE?)]
  [ id (name symbol?)]
  [ fun (param symbol?) (body FAE?)]
  [ app (fun-expr FAE?) (arg-expr FAE?)])

(define-type FAE-Value
  [ numV (n number?)]
  [ closureV (param symbol?)
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
(define (num+ n1 n2)
  ( numV (+ (numV-n n1) (numV-n n2))))

;; interp : FAE DefrdSub â†’ FAE-Value
(define (interp expr ds)
  (type-case FAE expr
    [ num (n) ( numV n)]
    [ add (l r) (num+ (interp l ds) (interp r ds))]
    [ id (v) (lookup v ds)]
    [ fun (bound-id bound-body)
          ( closureV bound-id bound-body ds)]
    [ app (fun-expr arg-expr)
          (local ([define fun-val (interp fun-expr ds)])
            (interp (closureV-body fun-val)
                    ( aSub (closureV-param fun-val)
                           (interp arg-expr ds)
                           (closureV-ds fun-val))))]))

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (first sexp)
; modify by parsing the following into a binop
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
; modify by parsing the following into an app with multiple params
;       [(with) (with (first (second sexp))
;                     (parse (second (second sexp)))
;                     (parse (third sexp)))
;               ]
; modify by parsing the following to handle multiple params
       [(fun) (fun 
               (first (second sexp))
               (parse (third sexp))
               )
              ]
       [else
        (cond ((list? (first sexp))  ;; this cond may not be needed
; modify by parsing the following to handle multiple params
               (app (parse (first sexp)) (parse (second sexp)))))
        ]
       )]))

;; testing the interpreter on a function application (single param)
;(interp (parse '{{fun {x} {+ x x}} 5}) (mtSub) )