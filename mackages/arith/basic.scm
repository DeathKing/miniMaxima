;;; BasicArith.scm
;;;
;;; Simple and basic arithmetic system.

(define TagExpr/New list)

(define (TagExpr/Tag expr) (list-ref expr 0))

(define (Expr/IsTagExpr? expr)
  (and (pair? expr) (> (length expr) 1) (symbol? (car expr))))

(define (Expr/TaggedExpr? expr tag)
  (and (Expr/IsTagExpr) (eq? (car expr) tag)))

(define (Expr/IsAtom? expr)
  (and (not (pair? expr)) (not (null? expr))))

(define (AddExpr/New   a b) (TagExpr/New 'add a b))
(define (SubExpr/New   a b) (TagExpr/New 'sub a b))
(define (MulExpr/New   a b) (TagExpr/New 'mul a b))
(define (DivExpr/New   a b) (TagExpr/New 'div a b))
(define (ExptExpr/New  a b) (TagExpr/New 'expt a b))
(define (FuncExpr/New  f a) (TagExpr/New 'func a))

(define (TagExpr/AddExpr? expr) (Expr/TaggedExpr? expr 'add))
(define (TagExpr/SubExpr? expr) (Expr/TaggedExpr? expr 'sub))
(define (TagExpr/MulExpr? expr) (Expr/TaggedExpr? expr 'mul))
(define (TagExpr/DivExpr? expr) (Expr/TaggedExpr? expr 'div))
(define (TagExpr/ExptExpr? expr) (Expr/TaggedExpr? expr 'expt))
(define (TagExpr/FuncExpr? expr) (Expr/TaggedExpr? expr 'func))

; TODO: MakeSumExpr should better be a syntax
(define (MakeSumExpr . args)
  (cond ((= 1 (length args)) args)
        ((= 2 (length args)) (AddExpr/New (car args) (cadr args)))
        (else
          (apply SumExpr/New
                 (cons (AddExpr/New (car args) (cadr args)) (cddr args))))))

(define (AddExpr/LHS expr) (list-ref expr 1))
(define (AddExpr/RHS expr) (list-ref expr 2))

(define (SubExpr/LHS expr) (list-ref expr 1))
(define (SubExpr/RHS expr) (list-ref expr 2))

(define (MulExpr/LHS expr) (list-ref expr 1))
(define (MulExpr/RHS expr) (list-ref expr 2))

(define (DivExpr/LHS expr) (list-ref expr 1))
(define (DivExpr/RHS expr) (list-ref expr 2))

(define (ExptExpr/Base expr) (list-ref expr 1))
(define (ExptExpr/Exponent expr) (list-ref expr 2))

(define (FuncExpr/Fx expr) (list-ref expr 1))
(define (FuncExpr/Args expr) (list-ref expr 2))

(define (TagExpr/SexprToExpr sexpr)
  (cond ((or (null? sexpr) (symbol? sexpr) (number? sexpr))
         sexpr)
        ((and (= (length sexpr) 3) (eq? (car sexpr) '+))
         (AddExpr/New (TagExpr/SexprToExpr (cadr sexpr))
                      (TagExpr/SexprToExpr (caddr sexpr))))
        ((and (= (length sexpr) 3) (eq? (car sexpr) '-))
         (SubExpr/New (TagExpr/SexprToExpr (cadr sexpr))
                      (TagExpr/SexprToExpr (caddr sexpr))))
        ((and (= (length sexpr) 3) (eq? (car sexpr) '*))
         (MulExpr/New (TagExpr/SexprToExpr (cadr sexpr))
                      (TagExpr/SexprToExpr (caddr sexpr))))
        ((and (= (length sexpr) 3) (eq? (car sexpr) '/))
         (DivExpr/New (TagExpr/SexprToExpr (cadr sexpr))
                      (TagExpr/SexprToExpr (caddr sexpr))))
        ((and (= (length sexpr) 3) (eq? (car sexpr) '^))
         (ExptExpr/New (TagExpr/SexprToExpr (cadr sexpr))
                       (TagExpr/SexprToExpr (caddr sexpr))))
        (else
         (FuncExpr/New (TagExpr/SexprToExpr (car sexpr))
                       (TagExpr/SexprToExpr (cadr sexpr))))))


