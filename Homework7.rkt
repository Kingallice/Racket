#lang plai

(define (operator-conv obj)
    (cond
        [(equal? obj '+) +]
        [(equal? obj '-) -]
        [(equal? obj '*) *]
        [(equal? obj '/) /]))

(define (bind-and-interp bound-id named-expr ds)
  (local ([define value-holder (box (numV 1729))]
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

(define-type FAE-Value
  [ numV (n number?)]
  [ closureV (params (listof symbol?))
             (body FAE?)
             (ds DefrdSub?)])

(define (boxed-FAE-value? v)
  (and (box? v)
       (FAE-Value? (unbox v))))

(define-type DefrdSub
  [ mtSub ]
  [ aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)]
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

;; num+ : numV numV âˆ’â†’ numV
(define (numop o n1 n2)
  ( numV (o (numV-n n1) (numV-n n2))))

;; interp : FAE DefrdSub â†’ FAE-Value
(define (interp expr ds)
  (type-case FAE expr
    [ num (n) ( numV n)]
    [ binop (o l r) (numop o (interp l ds) (interp r ds))]
    [ id (v) (lookup v ds)]
    [ if0 (c t e) (if (equal? (numV 0) (interp c ds)) (interp t ds) (interp e ds))]
    [ rec (bound-id named-expr bound-body) 
          (interp bound-body
            (bind-and-interp bound-id named-expr ds))]
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
       [(rec) 
          (rec (first (second sexp)) (parse (second (second sexp))) (parse (third sexp)))]
       [else
          (app (parse (first sexp)) (map (lambda (x) (parse x)) (rest sexp)))
        ]
       )]))

;; Part 1
(printf "\nPart 1 Test\n")
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

;; Part 2
(printf "\nPart 2 Test\n")
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
 (numV 120))

;; Part 3
(printf "\nPart 3 - Ackermann Function\n")
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
  (numV 3))

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
  (numV 9))

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
  (numV 13))

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
      {ack 4 1}})
    [mtSub])
  (numV 65533))