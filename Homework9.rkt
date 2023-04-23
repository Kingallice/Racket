#lang plai

(define-type FAE
  [ num (n number?)]
  [ add (lhs FAE?) (rhs FAE?)]
  [ id (name symbol?)]
  [ fun (param symbol?) (body FAE?)]
  [ app (fun-expr FAE?) (arg-expr FAE?)]
  [ newbox (expr FAE?)]
  [ setbox (expr1 FAE?) (expr2 FAE?)]
  [ openbox (expr FAE?)]
  [ seqn (expr1 FAE?) (expr2 FAE?)])

(define-type FAE-Value
  [ numV (n number?)]
  [ closureV (param symbol?)
             (body FAE?)
             (ds DefrdSub?)]
  [ boxV (location number?)])

(define-type ValueXStore
  [ vxs (value FAE-Value?) (store Store?)])

(define-type DefrdSub
  [ mtSub ]
  [ aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

(define-type Store
  [ mtSto ]
  [ aSto (location number?) (value FAE-Value?) (store Store?)])

;; lookup : symbol DefrdSub â†’ FAE-Value
(define (ds-lookup name ds)
  (type-case DefrdSub ds
    [ mtSub () (error 'ds-lookup "no binding for identifier" )]
    [ aSub (bound-name bound-value rest-ds)
           (if (symbol=? bound-name name)
               bound-value
               (ds-lookup name rest-ds))]))

(define (store-lookup loc-index sto)
  (type-case Store sto
    [ mtSto () (error 'store-lookup "no value at location" )]
    [ aSto (location value rest-store)
           (if (= location loc-index)
               value
               (store-lookup loc-index rest-store))]))

;; num+ : numV numV âˆ’â†’ numV
(define (num+ n1 n2)
  ( numV (+ (numV-n n1) (numV-n n2))))

;; interp : FAE DefrdSub â†’ FAE-Value
(define (interp expr ds store)
  (type-case FAE expr
    [ num (n) (vxs ( numV n) store)]
    [ id (v) (vxs (store-lookup (ds-lookup v ds) store) store)]
    [ add (l r) 
          (type-case ValueXStore (interp l ds store)
            [ vxs (l-value l-store)
                (type-case ValueXStore (interp r ds l-store)
                  [ vxs (r-value r-store)
                    (vxs (num+ l-value r-value) r-store)])])]
    [ fun (bound-id bound-body)
          ( closureV bound-id bound-body ds)]
    [ app (fun-expr arg-expr)
          (local ([define fun-val (interp fun-expr ds)])
            (interp (closureV-body fun-val)
                    ( aSub (closureV-param fun-val)
                           (interp arg-expr ds)
                           (closureV-ds fun-val))))]
    [ newbox (value-expr)
          (type-case ValueXStore (interp value-expr ds store)
            [vxs (expr-value expr-store)
                (local ([define new-loc (next-location store)])
                    (vxs (boxV new-loc)
                          (aSto new-loc expr-value expr-store)))])]))

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

;; Part 1 Test Code
  (test (parse '{with {b {newbox 0}}
                    {seqn {setbox b {+ 1 {openbox b}}}
                          {openbox b}}})
      (app
       (fun
        'b
        (seqn
         (setbox (id 'b) (add (num 1) (openbox (id 'b))))
         (openbox (id 'b))))
       (newbox (num 0))))

  (test (parse '{with {b {newbox 0}}
                    {if0 {seqn {setbox b 5}
                               {openbox b}}
                         1
                         {openbox b}}})
      (app
       (fun
        'b
        (if0
         (seqn (setbox (id 'b) (num 5)) (openbox (id 'b)))
         (num 1)
         (openbox (id 'b))))
       (newbox (num 0))))

  (test (interp (parse '{with {b {newbox 0}}
                            {seqn {setbox b {+ 1 {openbox b}}}
                                  {openbox b}}})
              (mtSub) (mtSto))
      (vxs (numV 1) (aSto 0 (numV 1) (aSto 1 (boxV 0) (aSto 0 (numV 0) (mtSto))))))

  (test (interp (parse '{with {b {newbox 0}}
                            {if0 {seqn {setbox b 5}
                                       {openbox b}}
                                 1
                                 {openbox b}}})
              (mtSub) (mtSto))
      (vxs (numV 5) (aSto 2 (numV 5) (aSto 3 (boxV 2) (aSto 2 (numV 0) (mtSto))))))

  (test
    (parse '{with {switch {newbox 0}}
               {with {toggle {fun {dum}
                                  {if0 {openbox switch}
                                       {seqn
                                        {setbox switch 1}
                                        1}
                                       {seqn
                                        {setbox switch 0}
                                        0}}}}
                     {+
                      {toggle 1729}
                      {toggle 1729}
                      }
                     }})
    (app
     (fun
      'switch
      (app
       (fun 'toggle (add (app (id 'toggle) (num 1729)) (app (id 'toggle) (num 1729))))
       (fun 'dum (if0 (openbox (id 'switch)) (seqn (setbox (id 'switch) (num 1)) (num 1)) (seqn (setbox (id 'switch) (num 0)) (num 0))))))
   (newbox (num 0))))

  (test (interp
       (parse '{with {switch {newbox 0}}
                     {with {toggle {fun {dum}
                                        {if0 {openbox switch}
                                             {seqn
                                              {setbox switch 1}
                                              1}
                                             {seqn
                                              {setbox switch 0}
                                              0}}}}
                           {+
                            {toggle 1729}
                            {toggle 1729}
                            }
                           }})
       (mtSub) (mtSto))
      (vxs
       (numV 1)
       (aSto
        4
        (numV 0)
        (aSto
         8
         (numV 1729)
         (aSto
          4
          (numV 1)
          (aSto
           7
           (numV 1729)
           (aSto
            6
            (closureV
             'dum
             (if0
              (openbox (id 'switch))
              (seqn (setbox (id 'switch) (num 1)) (num 1))
              (seqn (setbox (id 'switch) (num 0)) (num 0)))
             (aSub 'switch 5 (mtSub)))
            (aSto 5 (boxV 4) (aSto 4 (numV 0) (mtSto))))))))))

;;Part 2 Test Code
  (test (parse '{with {y 0}
                    {seqn
                     {seqn
                      {setvar y {+ y 5}}
                      {setvar y {+ y 15}}}
                     {seqn
                      {setvar y {+ y -3}}
                      y}}})
      (app
       (fun
        'y
        (seqn
         (seqn (setvar 'y (add (id 'y) (num 5))) (setvar 'y (add (id 'y) (num 15))))
         (seqn (setvar 'y (add (id 'y) (num -3))) (id 'y))))
       (num 0)))

  (test (interp
       (parse '{with {y 0}
                     {seqn
                      {seqn
                       {setvar y {+ y 5}}
                       {setvar y {+ y 15}}}
                      {seqn
                       {setvar y {+ y -3}}
                       y}}})
       (mtSub) (mtSto))
      (vxs
       (numV 17)
       (aSto 9 (numV 17) (aSto 9 (numV 20) (aSto 9 (numV 5) (aSto 9 (numV 0) (mtSto)))))))
