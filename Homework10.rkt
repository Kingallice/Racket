#lang plai

(define-type FAE ...) ;; same except for fun which will have the argument type

;; used to bind types to identifiers
(define-type Type
  [numT]
  [arrowT (arg Type?) (result Type?)])

;; used for type checking
(define-type TypeEnv
  [mtTypeEnv]
  [bindType (name symbol?) (type Type?) (rest TypeEnv?)])

(define (to-string x)
  (format "~a" x))

(define (type-error fae msg)
  (error 'no-type (format "~a not ~a" fae msg)))

;; iterate through the type bindings looking for the identifier.
;; return the type if found.  Very similar to lookup
(define (type-lookup name-to-find env)
  (type-case TypeEnv env
    [mtTypeEnv () (error 'free-variable (format "~a so no type" name-to-find))]
    [bindType ...]))

;; Check that the types of each item in 'exprs' matches the value in 'type
;; according to the binding in the type environment 'typenv'
;; If that passes, return the type passed in for result-type
(define (type-assert exprs type typenv result-type)
  (cond
    [(empty? exprs) result-type]
    [(not (equal? (typecheck (first exprs) typenv) type))
     (type-error (first exprs)
                 (to-string type)
                 )]
    [else (type-assert (rest exprs) type typenv result-type)]))

(define (type-of expr)
  (typecheck expr (mtTypeEnv)))

;; return the type of the FAE expression according to the binding in the
;; binding environment 'env'
(define (typecheck expr env)
  ;(display "typecheck expr=")(display expr)(newline)
  (type-case FAE expr
    ;; return the type of a num
    [ num (n) ...]
    ;; call type assert on a list containing the left and right parts, a numT, env, and a numT
    [ add (l r) ...]
    ;; simply do a type lookup on the identifier
    [ id (name) ...]
    ;; return an arrowT from the argument type to the type of the body
    ;; get the type of the body by calling typecheck with the body and
    ;; and the function name and arg-type added as a binding to the binding env
    [ fun (name arg-type body)
          (arrowT ...)]
    ;; determine the result type of the app
    [ app (fn arg)
          (type-case Type (typecheck fn env)
            [arrowT (arg-type result-type)
                    (type-assert (list arg) arg-type env result-type)]
            [else (type-error fn "function")])]
    ))

(define-type FAE-Value ...) ;; same
(define-type DefrdSub ...) ;; same
(define (lookup name ds) ...) ;; same
(define (num+ n1 n2)...) ;; same
(define (interp expr ds) ...) ;; same

;; parse a type expression to create either numT or recursively create arrowT's
;; if it's an arrowT, make sure that the syntax is a properly formed 
;; parse-type : sexpr -> Type
;; give structure
(define (parse-type sexp)
  ;(display "parse-type sexp=")(display sexp)(newline)
  (case sexp
    [(number) ...]
    [else
     (cond ((...) ; verify that it's a proper arrow syntax
            (arrowT ...)) ;; create a proper arrowT from the parts of the sexp, recursively calling parse-type on the parts
           (#t
            (error "undefined type:" sexp)))
     ]
    ))

;; update fun to include types
;; add an optional paramter typenv to dynamically determine the variable type in a with
(define (parse sexp [typenv (mtTypeEnv)])
  (cond
    [(number? sexp) ...] ;; same
    [(symbol? sexp) ...] ;; same
    [(list? sexp)
     (case (first sexp)
       [(+) ...] ;; same
       ;; create an app from a 'with'. dynamically determine the argument/parameter type
       ;; by calling typecheck on the argument.  use this arg-type when declaring the fun
       ;; and also add a new binding onto env from the name to the arg-type and pass that in
       ;; when parsing the body of the with (so that the type of the argument is available)
       [(with)  
        (app (fun
              ... ; name
              ... ; arg-type
              (parse ... ; body
                     (bindType ...)) ;;;binding for the name
              )
             ... ; argument fun is applied to
             )
        ]
       ;; modify fun to include the argument type (use parse-type)
       [(fun) ...]
       ;; this this point, the else will verify that this is a proper app (with two elements in a list
       ;; if it's not, signal a parse error with (error 'parse-error)))
       [else ...]
       )]
    [else ...]  ;; signal a parse error if reaching this point
    ))

;!!!!!!!!!!!! these two functions are for testing:
(define (run sexp) (interp (parse sexp) (mtSub)))
(define (typeof sexp) (type-of (parse sexp)))

;; general tests
(test (run '5 ) (numV 5) )
(test (parse '{+ 1 2}) (add (num 1) (num 2)))

;; check for erroneous forms in the parser
;; calling (error 'parse-error) when found.
(test/exn (parse "foo") "parse-error")
(test/exn (parse '{foo}) "parse-error")

(test (typeof '{+ 1 2}) (numT))
(test/exn (typeof 'x) "free-variable")

(test (parse '{fun {x : number} {+ x x}})
      (fun 'x (numT) (add (id 'x) (id 'x))))

(test (typeof '{fun {x : number} {+ x x}})
      (arrowT (numT) (numT)))

(test (parse '{fun {x : number} {fun {y : number} x}})
      (fun 'x (numT) (fun 'y (numT) (id 'x))))

(test (typeof '{fun {x : number} {fun {y : number} x}})
      (arrowT (numT) (arrowT (numT) (numT))))

(test/exn (typeof '{1 2})
          "no-type")  ;can match all or part of the exception

(test/exn (typeof '{+ {fun {x : number}  12} 2}) "no-type")

(test/exn (typeof '{{fun {f : {number -> number}}
                         {f 1}} 1})
          "no-type")

(test (parse '{{fun {x : number}
                    {{fun {f : {number -> number}}
                          {+ {f 1}
                             {{fun {x : number}
                                   {f 2}}
                              3}}}
                     {fun {y : number} {+ x y}}}}
               0})
      (app (fun 'x (numT)
                (app (fun 'f
                          (arrowT (numT) (numT))
                          (add
                           (app (id 'f) (num 1))
                           (app (fun 'x (numT) (app (id 'f) (num 2))) (num 3))))
                     (fun 'y (numT) (add (id 'x) (id 'y)))))
           (num 0)))

(test
 (run '{{fun {x : number}
             {{fun {f : {number -> number}}
                   {+ {f 1}
                      {{fun {x : number}
                            {f 2}}
                       3}}}
              {fun {y : number} {+ x y}}}}
        0})
 (numV 3))

(test
 (parse '{{fun {x : number} {+ x 12}} {+ 1 17}})
 (app (fun 'x (numT) (add (id 'x) (num 12))) (add (num 1) (num 17))))

(test (run '{{fun {x : number} {+ x 12}} {+ 1 17}})
      (numV 30))

(test (typeof '{{fun {x : number} {+ x 12}} {+ 1 17}})
      (numT))

(test (parse '{{{fun {x : number}
                     {fun {y : number}
                          {+ x y}}}
                5} 6})
      (app (app (fun 'x (numT)
                     (fun 'y (numT)
                          (add (id 'x) (id 'y)))) (num 5)) (num 6)))

(test
 (run '{{{fun {x : number}
              {fun {y : number}
                   {+ x y}}}
         5} 6})
 (numV 11))

(test
 (parse '{with {x 5} {+ x 2} })
 (app (fun 'x (numT) (add (id 'x) (num 2))) (num 5)))

(test (typeof '{with {x 5} {+ x 2} }) (numT))
(test (run '{with {x 5} {+ x 2} }) (numV 7))

(test
 (parse '{with {f {fun {n : number} {+ n n}}}
               {f 2}})
 (app
  (fun 'f (arrowT (numT) (numT)) (app (id 'f) (num 2)))
  (fun 'n (numT) (add (id 'n) (id 'n)))))

(test (typeof '{with {f {fun {n : number} {+ n n}}}
                     {f 2}})
      (numT))
      
(test (run '{with {f {fun {n : number} {+ n n}}}
                  {f 2}})
      (numV 4))

(test (parse '{with {f {fun {n : number} {+ n n}}}
                    {with {g {fun {h : (number -> number)} f}}
                          {g f}}})
      (app
       (fun
        'f
        (arrowT (numT) (numT))
        (app
         (fun
          'g
          (arrowT (arrowT (numT) (numT)) (arrowT (numT) (numT)))
          (app (id 'g) (id 'f)))
         (fun 'h (arrowT (numT) (numT)) (id 'f))))
       (fun 'n (numT) (add (id 'n) (id 'n)))))

(test
 (typeof '{with {f {fun {n : number} {+ n n}}}
                {with {g {fun {h : (number -> number)} f}}
                      {g f}}})
 (arrowT (numT) (numT)))

(test 
 (run '{with {f {fun {n : number} {+ n n}}}
             {with {g {fun {h : (number -> number)} f}}
                   {g f}}})
 (closureV 'n (add (id 'n) (id 'n)) (mtSub)))

;; extra credit tests:
;; parser tests
#|
(test/exn (parse '{+ 1})
          "wrong number of operands")

(test/exn (parse '{fun {x} x})
          "malformed fun expression")

(test/exn (parse '{fun x : number x})
          "expected a typed formal parameter")
             
(test/exn (parse '{fun {x} : number x})
          "malformed typed formal parameter")

(test/exn (parse '{fun {1 : number} : number x})
          "expected an identifier")

;; parse-type tests
(test/exn (parse '{fun {x : 1} : 1 x})
          "expected a type expression")
             
(test/exn (parse '{fun {x : {number}} : number {x 1}})
          "malformed type expression")
             
(test/exn (parse '{fun {x : foo} : foo x})
          "unknown type: foo")
             
(test/exn (typeof 'x) "free identifier: x")
             
(test/exn (typeof '{+ 1 {fun {x : number} : number x}})
          "type mismatch: operands must be numbers")
             
(test/exn (typeof '{fun {x : number} : (number -> number) x})
          "type mismatch: function body has wrong type")
             
(test/exn (typeof '{{fun {f : (number -> number)} : number 0} 0})
          "type mismatch: actual parameter has wrong type")

(test/exn (typeof (parse '{1 2}))
          "expected a function")
|#