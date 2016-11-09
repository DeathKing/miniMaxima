(load-relative "poly.scm")

(define (Poly/SameVariable? atom var)
  (and (and (symbol? var) (symbol? atom))
       (eq? var atom)))

(define (Poly/FunctionOf? poly var)
  (if (AlgeExpr/Atom? poly)
      (Poly/SameVariable? poly var)
      (let ((tag (AlgeExpr/Tag poly)))
        (cond ((eq? tag 'add)
               (or (Poly/FunctionOf? (AddExpr/LHS poly) var)
                   (Poly/FunctionOf? (AddExpr/RHS poly) var)))
              ((eq? tag 'sub)
               (or (Poly/FunctionOf? (SubExpr/LHS poly) var)
                   (Poly/FunctionOf? (SubExpr/RHS poly) var)))
              ((eq? tag 'mul)
               (or (Poly/FunctionOf? (MulExpr/LHS poly) var)
                   (Poly/FunctionOf? (MulExpr/RHS poly) var)))
              ((eq? tag 'div)
               (or (Poly/FunctionOf? (DivExpr/LHS poly) var)
                   (Poly/FunctionOf? (DivExpr/RHS poly) var)))
              ((eq? tag 'expt)
               (and (Poly/FunctionOf? (ExptExpr/Base poly) var)
                    (not (Poly/FunctionOf? (ExptExpr/Exponent poly) var))))
              (else
                (error "Not implemented."))))))

;;; Assume poly are fully expanded
;;; not correct for symbolic computation.
(define (Poly/Const poly var)
  (cond ((number? poly) poly)                    ; 5 -> 5
        ((and (symbol? poly) (eq? poly var)) 0)  ; x -> 0
        ((symbol? poly) poly)                    ; a -> a
        ((AlgeExpr/AddExpr? poly)
         (MakeAddExpr (Poly/Const (AddExpr/LHS poly) var)
                      (Poly/Const (AddExpr/RHS poly) var)))
        ((AlgeExpr/SubExpr? poly)
         (MakeSubExpr (Poly/Const (SubExpr/LHS poly) var)
                      (Poly/Const (SubExpr/RHS poly) var)))
        ((AlgeExpr/MulExpr? poly)
         (MakeMulExpr (Poly/Const (MulExpr/LHS poly) var)
                      (Poly/Const (MulExpr/RHS poly) var)))
        ((AlgeExpr/DivExpr? poly)
         (MakeDivExpr (Poly/Const (DivExpr/LHS poly) var)
                      (Poly/Const (DivExpr/RHS poly) var)))))
        