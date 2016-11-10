(define (PolyExpr/FunctionOf? poly var)
  (if (TagExpr/Atom? poly)
      (eq? poly var)
      (let ((tag (TagExpr/Tag poly)))
        (cond ((eq? tag 'add)
               (or (PolyExpr/FunctionOf? (AddExpr/LHS poly) var)
                   (PolyExpr/FunctionOf? (AddExpr/RHS poly) var)))
              ((eq? tag 'sub)
               (or (PolyExpr/FunctionOf? (SubExpr/LHS poly) var)
                   (PolyExpr/FunctionOf? (SubExpr/RHS poly) var)))
              ((eq? tag 'mul)
               (or (PolyExpr/FunctionOf? (MulExpr/LHS poly) var)
                   (PolyExpr/FunctionOf? (MulExpr/RHS poly) var)))
              ((eq? tag 'div)
               (or (PolyExpr/FunctionOf? (DivExpr/LHS poly) var)
                   (PolyExpr/FunctionOf? (DivExpr/RHS poly) var)))
              ((eq? tag 'expt)
               (and (not (PolyExpr/FunctionOf? (ExptExpr/Base poly) var))
                    (not (PolyExpr/FunctionOf? (ExptExpr/Exponent poly) var))))
              (else
                (error "Not implemented."))))))

;;; Assume poly are fully expanded
;;; return const expr respect to a var
(define (PolyExpr/ConstExpr poly var)
  (cond ((number? poly) poly)                    ; 5 -> 5
        ((and (symbol? poly) (eq? poly var)) 0)  ; x -> 0
        ((symbol? poly) poly)                    ; a -> a
        ((TagExpr/AddExpr? poly)
         (AddExpr/New (PolyExpr/ConstExpr (AddExpr/LHS poly) var)
                      (PolyExpr/ConstExpr (AddExpr/RHS poly) var)))
        ((TagExpr/SubExpr? poly)
         (SubExpr/New (PolyExpr/ConstExpr (SubExpr/LHS poly) var)
                      (PolyExpr/ConstExpr (SubExpr/RHS poly) var)))
        ((TagExpr/MulExpr? poly)
         (MulExpr/New (PolyExpr/ConstExpr (MulExpr/LHS poly) var)
                      (PolyExpr/ConstExpr (MulExpr/RHS poly) var)))
        ((TagExpr/DivExpr? poly)
         (DivExpr/New (PolyExpr/ConstExpr (DivExpr/LHS poly) var)
                      (PolyExpr/ConstExpr (DivExpr/RHS poly) var)))
        ((TagExpr/ExptExpr? poly)
         (if (PolyExpr/FunctionOf? poly var)
             (error "The original expression contains a none-poly part.")
             poly))
        (else
          (error "Not implemented yet."))))


        