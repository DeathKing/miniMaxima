(require-relative "basic.scm")
(require-relative "poly.scm")

(define (DeriveExpr/New fx var)
  (list 'derive fx var))

(define (DeriveExpr/Fx expr) (list-ref expr 1))
(define (DeriveExpr/Var expr) (list-ref expr 2))

;;; Do derive in syntax category
(define (derive expr var)
  (cond ((not (symbol? var))
         (error "Var must be a symbol -- " var))
        ((number? expr) 0) ; const
        ((symbol? expr)    ; x' -> 1, a' -> 0
         (if (eq? expr var) 1 0))
        ((TagExpr/AddExpr? expr)
         (AddExpr/New (derive (AddExpr/LHS expr) var)
                      (derive (AddExpr/RHS expr) var)))
        ((TagExpr/SubExpr? expr)
         (SubExpr/New (derive (SubExpr/LHS expr) var)
                      (derive (SubExpr/RHS expr) var)))
        ((TagExpr/MulExpr? expr)
         (AddExpr/New (MulExpr/New (derive (MulExpr/LHS expr) var)
                               (MulExpr/RHS expr) var)
                  (MulExpr/New (MulExpr/LHS expr)
                               (derive (MulExpr/RHS expr) var))))
        ((TagExpr/DivExpr? expr)
         (DivExpr/New
           (SubExpr/New (MulExpr/New (derive (DivExpr/LHS expr) var)
                                     (DivExpr/RHS expr))
                        (MulExpr/New (DivExpr/LHS expr)
                                     (derive (DivExpr/RHS expr) var)))
           (ExptExpr/New (DivExpr/RHS expr) 2)))
        ((TagExpr/ExptExpr? expr)
         (cond ((eq? (ExptExpr/Base expr) 'e)
                (MulExpr/New expr (derive (ExptExpr/Exponent expr) var)))
               ((PolyExpr/FunctionOf? (ExptExpr/Exponent expr) var)
                (error "No rule to derive such expr -- e^f(var)."))
               (else
                 (MulExpr/New
                   (MulExpr/New (SubExpr (ExptExpr/Exponent expr) 1)
                                expr)
                   (derive (ExptExpr/Base expr) var)))))
        (else
          (error "No rule to derive such expr."))))

 