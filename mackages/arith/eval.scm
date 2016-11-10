(require-relative "basic.scm")
(require-relative "env.scm")

(define *gEvTable* '())

(define (InstallEvalProcedureGlobal! tag p)
  (set! *gEvTable* (Env/Extend tag p *gEvTable*)))

(define (InstallEvalProcedure tag p evtable)
  (Env/Extend tag p *gEvTable*))

;;; Expr/EvalAtom
;;;
;;; Evaluate a atom expression
(define (AtomExpr/Eval expr env)
  (cond ((number? expr) expr)
        ((symbol? expr) (Env/Lookup expr env))
        (else
          (error "expr is not a atom expression -- " expr))))

(define (TagExpr/Eval expr env)
  (if (TagExpr/Atom? expr)
      (AtomExpr/Eval expr env)
      (let ((evp (Env/Lookup (TagExpr/Tag expr) *gEvTable*)))
        (evp expr env))))

(define (AddExpr/Eval expr env)
  (let ((lhs (TagExpr/Eval (AddExpr/LHS expr) env))
        (rhs (TagExpr/Eval (AddExpr/RHS expr) env))
        (add (TagExpr/Eval 'add env)))
    ; TODO: maybe poly-add
    (add lhs rhs)))

(define (SubExpr/Eval expr env)
  (let ((lhs (TagExpr/Eval (SubExpr/LHS expr) env))
        (rhs (TagExpr/Eval (SubExpr/RHS expr) env))
        (sub (TagExpr/Eval 'sub env)))
    ; TODO: maybe poly-sub
    (sub lhs rhs)))

(define (MulExpr/Eval expr env)
  (let ((lhs (TagExpr/Eval (MulExpr/LHS expr) env))
        (rhs (TagExpr/Eval (MulExpr/RHS expr) env))
        (mul (TagExpr/Eval 'mul env)))
    (mul lhs rhs)))

(define (DivExpr/Eval expr env)
  (let ((lhs (TagExpr/Eval (DivExpr/LHS expr) env))
        (rhs (TagExpr/Eval (DivExpr/RHS expr) env))
        (div (TagExpr/Eval 'div env)))
    (div lhs rhs)))

(define (ExptExpr/Eval expr env)
  (let ((base (TagExpr/Eval (ExptExpr/Base expr) env))
        (exp  (TagExpr/Eval (ExptExpr/Exponent expr) env))
        (expt (TagExpr/Eval 'expt env)))
    (expt base exp)))

(define (DeriveExpr/Eval expr env)
  (let ((fx (DeriveExpr/Fx expr))
        (var (DeriveExpr/Var expr)))
    (TagExpr/Eval (derive fx var) env)))

(define (FastExpt b n)
  (cond ((= n 0) 1)
        ((even? n)
         (let ((x (FastExpt b (/ n 2))))
           (* x x)))
        (else 
          (* b (FastExpt b (- n 1))))))

(define *DefaultEnv*
  (Env/MakeFromEntries (cons 'add +)
                       (cons 'sub -)
                       (cons 'mul *)
                       (cons 'div /)
                       (cons 'expt FastExpt)))

(InstallEvalProcedureGlobal! 'add  AddExpr/Eval)
(InstallEvalProcedureGlobal! 'sub  SubExpr/Eval)
(InstallEvalProcedureGlobal! 'div  DivExpr/Eval)
(InstallEvalProcedureGlobal! 'mul  MulExpr/Eval)
(InstallEvalProcedureGlobal! 'expt ExptExpr/Eval)

;(Env/Inspect *DefaultEnv*)

;(newline)

;; Test:
;; Eval( 3 + 2 == 5)
;(display (Expr/Eval (MakeAddExpr 3 2) *DefaultEnv*))

;(newline)

;; Eval( 2 * x + 3 == 9) where ((x . 3))
;(display (Expr/Eval (MakeAddExpr (MakeMulExpr 2 'x) 3)
;                        (Env/Extend 'x 3 *DefaultEnv*)))

;(newline)

;; Eval( 2 * x ^ 2 - 4 == 14) where ((x . 3))
;(display (Expr/Eval (MakeSubExpr (MakeMulExpr 2 (MakeExptExpr 'x 2)) 4)
;                        (Env/Extend 'x 3 *DefaultEnv*)))
