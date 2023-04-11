#lang plai

(define (operator-conv obj)
    (cond
        [(equal? obj '+) +]
        [(equal? obj '-) -]
        [(equal? obj '*) *]
        [(equal? obj '/) /]))

(define (bind-and-interp bound-id named-expr ds)
  (local ([define value-holder (box 1729)]
          [define new-ds (recSub bound-id value-holder ds)]
          [define named-expr-val (interp named-expr new-ds)])
    (begin (set-box! value-holder named-expr-val) new-ds)))

(define (build-subs params args tail)
  (if (null? params)
    tail
    (build-subs (rest params) (rest args) (aSub (first params) (first args) tail))))

(define-type FAE
  [ num (n number?)]
  [ binop (o procedure?) (lhs FAE?) (rhs FAE?)]
  [ id (name symbol?)]
  [ if0 (c FAE?) (t FAE?) (e FAE?) ]
  [ rec (bound-id symbol?) (named-expr FAE?) (bound-body FAE?)]
  [ fun (args (listof symbol?)) (body FAE?)]
  [ app (fun-expr FAE?) (args (listof FAE?))])

(define (boxed-FAE-value? v)
  (and (box? v)
       (number-or-procedure? (unbox v))))

(define (number-or-procedure? v)
  (or (number? v) (procedure? v)))

(define-type DefrdSub
  [ mtSub ]
  [ aSub (name symbol?) (value number-or-procedure?) (ds DefrdSub?)]
  [ recSub (name symbol?) (named-expr boxed-FAE-value?) (ds DefrdSub?)])

;; lookup : symbol DefrdSub â†’ FAE-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [ mtSub () (error 'lookup "no binding for identifier" )]
    [ aSub (bound-name bound-value rest-ds)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-ds))]
    [ recSub (bound-name bound-value rest-ds) 
           (if (symbol=? bound-name name)
               (unbox bound-value)
               (lookup name rest-ds))]
    ))

;; interp : FAE DefrdSub â†’ FAE-Value
(define (interp expr ds)
  (type-case FAE expr
    [ num (n) n]
    [ binop (o l r) (o (interp l ds) (interp r ds))]
    [ id (v) (lookup v ds)]
    [ if0 (c t e) (if (equal? 0 (interp c ds)) (interp t ds) (interp e ds))]
    [ rec (bound-id named-expr bound-body) 
          (interp bound-body
            (bind-and-interp bound-id named-expr ds))]
    [ fun (bound-id bound-body)
          (lambda (arg-vals) 
            (interp bound-body 
              (build-subs bound-id arg-vals ds)))]
    [ app (fun-expr args-expr)
          (local ([define fun-val (interp fun-expr ds)]
                  [define arg-vals (map (lambda (arg) (interp arg ds)) args-expr)])
              (fun-val arg-vals)
              )]))
                ;(interp fun-val (build-subs fun-val (map (lambda (arg) (interp arg ds)) args-expr) ds)))]))
          ;(printf "\nfun-expr: ~a\nargs: ~a\nfun-val: ~a\narg-vals: ~a\n" fun-expr args (interp fun-expr ds) 0); (interp args ds)) ; (interp args ds))
          ;((interp fun-expr ds) (interp args ds))]))

          ;(local ([define fun-val (interp fun-expr ds)]
           ;       [define arg-vals (interp args ds)]
            ;      [define print (printf "~a" fun-val)])
            ;(fun-val arg-vals)
            ;)]))

;    [ app (fun-expr args)
 ;         (local ([define fun-val (interp fun-expr ds)])
 ;           (interp (closureV-body fun-val)
 ;                   (build-subs (closureV-params fun-val)
  ;                         (map (lambda (arg) (interp arg ds)) args)
   ;                        (closureV-ds fun-val))
    ;                  ))]))

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
       [(with) (app
                 (parse (list 'fun (map (lambda (pair) (first pair)) (second sexp)) (third sexp)))
                 (map (lambda (pair) (parse (second pair))) (second sexp)))]
      ; if0 - (if0 cond then else) if cond == 0 -> then, otherwise -> else
       [(if0) (if0 (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)))]
      ; modify by parsing the following to handle multiple params
       [(fun) (fun 
                (second sexp)
                (parse (third sexp))
               )
              ]
       [(rec) ;(printf "Recursion")] 
          (rec (first (second sexp)) (parse (second (second sexp))) (parse (third sexp)))]
       [else
          ;(printf "App\n\t~a\n\n" (app (parse (first sexp)) (map (lambda (x) (parse x)) (rest sexp))))
          (app (parse (first sexp)) (map (lambda (x) (parse x)) (rest sexp)))
        ]
       )]))

;Testing Code
  ;; sample test cases
  (printf "HW6 Sample Test Cases\n")
    (test
      (interp (parse '5 ) [mtSub])
      5)
    
    (test
      (interp (parse '{{fun {x} {+ x x}} 5}) (mtSub))
      10)

  ;; HW6 Part 1
  (printf "\nHW6 Part 1 Test Cases\n")
    (test
      (parse '{* 1 2})
      (binop * (num 1) (num 2)))

    (test
      (interp (parse '{* 1 2}) (mtSub))
       2)

    (test
      (parse '{* 2 {/ 3 4}})
      (binop * (num 2) (binop / (num 3) (num 4))))

    (test
      (interp (parse '{* 2 {/ 3 4}}) (mtSub))
       (/ 3 2))

  ;; HW6 Part 2
  (printf "\nHW6 Part 2 Test Cases\n")
    (test
      (parse '{fun {x y z} {+ x x}})
      (fun '(x y z) (binop + (id 'x) (id 'x))))
  
  ;; HW6 Part 3
  (printf "\nHW6 Part 3 Test Cases\n")
    (test
      (interp (parse '{{fun {x y z} {* {+ x y} z}} 5 6 7}) (mtSub))
      77)

  ;; HW6 Part 4
  (printf "\nHW6 Part 4 Test Cases\n")
    (test
    (parse '{with {{x 5} {y 6} {z 7} } {* {+ x y} z} })
    (app (fun '(x y z) (binop * (binop + (id 'x) (id 'y)) (id 'z))) (list (num 5) (num 6) (num 7))))

    (test
    (interp (parse '{with {{x 5} {y 6} {z 7} } {* {+ x y} z} }) (mtSub))
     77)

  ;; HW6 Part 5
  (printf "\nHW6 Part 5 Test Cases\n")
    (test
    (parse '(if0 0 1 2))
    (if0 (num 0) (num 1) (num 2)))

    (test
    (interp (parse '(if0 0 1 2)) (mtSub))
    1)

    (test
    (parse '(if0 3 1 2))
    (if0 (num 3) (num 1) (num 2)))

    (test
    (interp (parse '(if0 3 1 2)) (mtSub))
    2)

    (test
    (parse '{with {{n 0}}
                  {if0 n
                        1
                        2}})
    (app (fun '(n) (if0 (id 'n) (num 1) (num 2))) (list (num 0))))

    (test
    (interp
      (parse '{with {{n 0}}
                    {if0 n
                        1
                        2}})
      [mtSub])
     1)

    (test
    (parse '{{fun {w x}
                  {with {{y 5} {z 6}}
                        (if0
                          {- (* w y) (* x z)}
                          {/ w y}
                          {/ x z})}}
              5 6})
    (app (fun '(w x)
              (app (fun '(y z)
                        (if0
                          (binop -
                                (binop * (id 'w) (id 'y))
                                (binop * (id 'x) (id 'z)))
                          (binop / (id 'w) (id 'y))
                          (binop / (id 'x) (id 'z))))
                    (list (num 5) (num 6))))
          (list (num 5) (num 6))))

  (test
    (interp (parse '{{fun {w x}
                          {with {{y 5} {z 6}}
                                (if0
                                  {- (* w y) (* x z)}
                                  {/ w y}
                                  {/ x z})}}
                      5 6})
            (mtSub))
    1)

  ;; HW7 Part 1
  (printf "\nHW7 Part 1 Test\n")
    (test
    (parse '{rec {fac
                  {fun {n}
                        {if0 n
                            1
                            {* n {fac {+ n -1}}}
                            }}}
              {fac 4}})
    (rec 'fac
      (fun '(n)
            (if0
            (id 'n)
            (num 1)
            (binop *
                    (id 'n)
                    (app (id 'fac) (list (binop + (id 'n) (num -1)))))))
      (app (id 'fac) (list (num 4)))))

  ;; HW7 Part 2
  (printf "\nHW7 Part 2 Test\n")
    (test
     (interp
      (parse '{rec {fac
                    {fun {n}
                        {if0 n
                              1
                              {* n {fac {+ n -1}}}
                              }}}
                {fac 5}})
      [mtSub])
     120)

  ;; HW7 Part 3
  (printf "\nHW7 Part 3 - Ackermann Function\n")
    (test
    (interp 
      (parse '{rec {ack
          {fun {m n}
            {if0 m
              {+ n 1}
              {if0 n
                {ack {- m 1} 1}
                {ack {- m 1} {ack m {- n 1}}}
              }
            }
          }
        }
        {ack 1 1}})
      [mtSub])
     3)

    (test
    (interp 
      (parse '{rec {ack
          {fun {m n}
            {if0 m
              {+ n 1}
              {if0 n
                {ack {- m 1} 1}
                {ack {- m 1} {ack m {- n 1}}}
              }
            }
          }
        }
        {ack 2 3}})
      [mtSub])
     9)

    (test
    (interp 
      (parse '{rec {ack
          {fun {m n}
            {if0 m
              {+ n 1}
              {if0 n
                {ack {- m 1} 1}
                {ack {- m 1} {ack m {- n 1}}}
              }
            }
          }
        }
        {ack 4 0}})
      [mtSub])
     13)