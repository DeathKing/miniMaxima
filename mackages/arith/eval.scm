(load-relative "basic-arith.scm")
(load-relative "env.scm")

(define *gEvTable* '())

(define (InstallEvalProcedureGlobal! tag p)
  (set! *gEvTable* (Env/Extend tag p *gEvTable*)))

(define (InstallEvalProcedure tag p evtable)
  (Env/Extend tag p *gEvTable*))

(define (AlgeExpr/Eval expr env)
  (if (AlgeExpr/Atom? expr)
      (AlgeExpr/EvalAtom expr env)
      (let ((evp (Env/Lookup (AlgeExpr/Tag expr) *gEvTable*)))
        (evp expr env))))

;;; AlgeExpr/EvalAtom
;;;
;;; Evaluate a atom expression
(define (AlgeExpr/EvalAtom expr env)
  (cond ((number? expr) expr)
        ((symbol? expr) (Env/Lookup expr env))
        (else (error "expr is not a atom expression -- " expr))))

(define (AlgeExpr/EvalAdd expr env)
  (let ((lhs (AlgeExpr/Eval (AddExpr/LHS expr) env))
        (rhs (AlgeExpr/Eval (AddExpr/RHS expr) env))
        (add (AlgeExpr/Eval 'add env)))
    ; TODO: maybe poly-add
    (add lhs rhs)))

(define (AlgeExpr/EvalSub expr env)
  (let ((lhs (AlgeExpr/Eval (SubExpr/LHS expr) env))
        (rhs (AlgeExpr/Eval (SubExpr/RHS expr) env))
        (sub (AlgeExpr/Eval 'sub env)))
    ; TODO: maybe poly-sub
    (sub lhs rhs)))

(define (AlgeExpr/EvalMul expr env)
  (let ((lhs (AlgeExpr/Eval (MulExpr/LHS expr) env))
        (rhs (AlgeExpr/Eval (MulExpr/RHS expr) env))
        (mul (AlgeExpr/Eval 'mul env)))
    (mul lhs rhs)))

(define (AlgeExpr/EvalDiv expr env)
  (let ((lhs (AlgeExpr/Eval (DivExpr/LHS expr) env))
        (rhs (AlgeExpr/Eval (DivExpr/RHS expr) env))
        (div (AlgeExpr/Eval 'div env)))
    (div lhs rhs)))

(define (AlgeExpr/EvalExpt expr env)
  (let ((base (AlgeExpr/Eval (ExptExpr/Base expr) env))
        (exp  (AlgeExpr/Eval (ExptExpr/Exponent expr) env))
        (expt (AlgeExpr/Eval 'expt env)))
    (expt base exp)))

(define (FastExpt b n)
  (cond ((= n 0) 1)
        ((even? n) (let ((x (FastExpt b (/ n 2)))) (* x x)))
        (else (* b (FastExpt b (- n 1))))))

(define *DefaultEnv*
  (MakeEnvFromEntries (cons 'add +)
                      (cons 'sub -)
                      (cons 'mul *)
                      (cons 'div /)
                      (cons 'expt FastExpt)))

(InstallEvalProcedureGlobal! 'add  AlgeExpr/EvalAdd)
(InstallEvalProcedureGlobal! 'sub  AlgeExpr/EvalSub)
(InstallEvalProcedureGlobal! 'div  AlgeExpr/EvalDiv)
(InstallEvalProcedureGlobal! 'mul  AlgeExpr/EvalMul)
(InstallEvalProcedureGlobal! 'expt AlgeExpr/EvalExpt)

;(Env/Inspect *DefaultEnv*)

;(newline)

;; Test:
;; Eval( 3 + 2 == 5)
;(display (AlgeExpr/Eval (MakeAddExpr 3 2) *DefaultEnv*))

;(newline)

;; Eval( 2 * x + 3 == 9) where ((x . 3))
;(display (AlgeExpr/Eval (MakeAddExpr (MakeMulExpr 2 'x) 3)
;                        (Env/Extend 'x 3 *DefaultEnv*)))

;(newline)

;; Eval( 2 * x ^ 2 - 4 == 14) where ((x . 3))
;(display (AlgeExpr/Eval (MakeSubExpr (MakeMulExpr 2 (MakeExptExpr 'x 2)) 4)
;                        (Env/Extend 'x 3 *DefaultEnv*)))
