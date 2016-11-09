;;; BasicArith.scm
;;;
;;; Simple and basic arithmetic system.

(define MakeAlgeExprWithTag list)

(define (AlgeExpr/Tag expr) (list-ref expr 0))

(define (AlgeExpr/Atom? expr)
  (and (not (pair? expr)) (not (null? expr))))

(define (MakeAddExpr a b)  (MakeAlgeExprWithTag 'add a b))
(define (MakeSubExpr a b)  (MakeAlgeExprWithTag 'sub a b))
(define (MakeDivExpr a b)  (MakeAlgeExprWithTag 'div a b))
(define (MakeMulExpr a b)  (MakeAlgeExprWithTag 'mul a b))
(define (MakeExptExpr a b) (MakeAlgeExprWithTag 'expt a b))
(define (MakeFuncExpr f a) (MakeAlgeExprWithTag 'func a))

(define (MakeSumExpr . args)
  (cond ((= 1 (length args)) args)
        ((= 2 (length args)) (MakeAddExpr (car args) (cadr args)))
        (else
          (apply MakeSumExpr
                 (cons (MakeAddExpr (car args) (cadr args)) (cddr args))))))

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

(define (Poly/Const expr))