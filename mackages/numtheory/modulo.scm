(load-relative "../arith/basic-arith.scm")

;(define (SolveConguModEqs eqs)
;  (let (eq (map UnifyConurenceModulo eqs))))

(define (MakeConguMod f m . from)
  (MakeAlgeExprWithTag 'congumod f m from))

(define (ConguModEq/Fx eq)            (list-ref eq 1))
(define (ConguModEq/Modulus eq)       (list-ref eq 2))
(define (ConguModEq/DecomposeFrom eq) (list-ref eq 3))
(define (ConguModEq/Const eq var)     (Poly/Const (ConguModeEq/Fx eq) var))

(define (MakeConguModEqs . eqs)
  (MakeAlgeExprWithTag 'conguod-eqs eqs))

(define (ConguModeEqs/Eqs eqs)
  (list-ref eqs 1))

;;; Mod/FindInv
;;;
;;; Theorem. element a has a inverse element in mod N, iff gcd(a, n) = 1
(define (Mod/FindInv a m)
  (let ((tri (egcd a m)))
    (let ((g (list-ref tri 0))
          (x (list-ref tri 1)))
      (if (= g 1)
          (modulo x m)
          '()))))

; Using Brute Force search the solve of a unary equation no matter how order
; it is.
(define (ConguModEq/SolveByBruteForce eq var env)
  (let* ((fx       (ConguModEq/Fx eq))
         (modulus  (ConguModEq/Modulus eq))
         (mod-add  (lambda (a b) (modulo (+ a b) modulus)))
         (mod-sub  (lambda (a b) (modulo (- a b) modulus)))
         (mod-mul  (lambda (a b) (modulo (* a b) modulus)))
         (mod-expt (lambda (a b) (modulo (FastExpt a b) modulus)))
         (env1     (MakeEnvFromEntries (cons 'add mod-add)
                                       (cons 'sub mod-sub)
                                       (cons 'mul mod-mul)
                                       (cons 'expt mod-expt)))
         (env2     (Env/Merge env1 (Env/Merge env *DefaultEnv*))))
    (filter (lambda (x) (not (null? x)))
            (map (lambda (x)
                   (let ((q (AlgeExpr/EvalExpr fx (Env/Extend var x env2))))
                     (if (= 0 q) x '())))
                 (iota (- modulus 1) 1)))))

(define (ConguModEqs/SolveByCRT eqs var env)
  ;;; Decompose the equations at first
  (define eqsp
    (fold-right append
                (map ConguModEq/DecomposeByModulus (ConguModEqs/Eqs eqs))))
  ;;; Check if them are compatible
  (if (ConguModEqs/Compatible? eqsp)
      (let ((m (apply product (map ConguModEq/Modulus eqsp))))
        (let ((s (apply sum 
                       (map (lambda (eq)
                              (let* ((eqm (ConguModEq/Modulus eq))
                                     (eqc (ConguModEq/Const eq var))
                                     (eqb (modulo (- c) m))
                                     (bigM (quotient m eqm))
                                     (eqy (Mod/FindInv bigM eqm)))
                                (* b bigM eqm)))
                            eqsp))))
          (modulo s m)))
      'error))


;;; ConguModEq/*ShowStyle*
;;;
;;; 1 -> the const of fx is on the right-hand-side like 3x - 5 ≡ 0 (mod 9)
;;; 2 -> the const of fx is on the left-hand-side like 3x ≡ 5 (mod 9)
;;; else -> unsupport
(define ConguModEq/*ShowStyle* 1)

(define ConguModEq/*SymConguEq* "≡")

(define (ConguModEq/DisplayASCII eq)
  'pass)

;;; ConguModeEq/DecomposeByModuli
;;;
;;; Decompose congurence so that their moduli are primes
(define (ConguModeEq/DecomposeByModulus eq)
  (let ((m  (ConguModEq/Modulus eq))
        (fx (ConguModEq/Fx eq)))
    (map
      (lambda (mp) (MakeConguMod fx mp eq))
      (list-unique (Prime/Factor m)))))


(define (ConguModeEqs/Compatible? eqs)
  (if (< (length eqs) 2)
      (error "Type error for length eqs must great than 1")
      (let ((e (sort eqs (lambda (x y) (< (ConguModEq/Modulus x)
                                          (ConguModEq/Modulus y))))))
        (let check ((ep e) (checked '()))
          (if (null? ep)
              #t
              (let* ((eq (car ep))
                     (entry (assoc (ConguModEq/Modulus eq) checked)))
                (cond ((and entry (= (ConguModEq/Fx entry)
                                     (ConguModEq/Fx eq)))
                       (check (cdr eq) checked))
                      (entry
                       (let ((errmsg (format #f
                                             "At least two equations are incompatible: ~% ~A ~%and~%~A"
                                             (ConguModEq/DisplayASCII ep)
                                             (ConguModEq/DisplayASCII entry))))
                         (error errmsg)))
                      (else
                       (check (cdr eq) (cons eq checked))))))))))
              
              
; x^5 + x + 1 = 0 \pmod 7
;(define eq (MakeConguMod (MakeSumExpr (MakeExptExpr 'x 5) 'x 1) 7))
;(display (ConguModEq/SolveByBruteForce eq 'x))

;(newline)

;(define eq2 (MakeConguMod (MakeSubExpr (MakeExptExpr 'x 25) 'x) 13))
;(display (ConguModEq/SolveByBruteForce eq2 'x))
