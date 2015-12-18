;;; SEXP.scm
;;;
;;; The intermediate representation of (simple) algebra expression.

(define (exp/s-exp? s)
  (list? s))

(define (exp/atom? s)
  (not (pair? s)))

; exp/simple-operation?
; a expression consisdered to be simple,
; iff it's a binary operation and arguments both are atom.
(define (exp/simple-operation? s)
  (and (not (exp/atom? s))
       (exp/atom? (arg1 s))
       (exp/atom? (arg2 s))))

; exp/simple-expression?
; both atom and simple-operation considered to be simple-expression
(define (exp/simple-expression? s)
  (or (exp/atom? s)
      (and (exp/atom? (arg1 s))
           (exp/atom? (arg2 s)))))

; These operator assume that the s is s-exp
(define (exp/operator s) (car s))
(define (exp/arg1 s)     (cadr s))
(define (exp/arg2 s)     (caddr s))

(define *root*         'root)
(define *power*        'power)
(define *negtive*      'neg)
(define *pos-nor-neg*  'pon)
(define *parenthsises* 'paren)

(define (exp/sum? s)
  (and (not (exp/atom? s))
       (eq? (operator s) '+)))

(define (exp/sub? s)
  (and (not (exp/atom? s))
       (eq? (operator s) '-)))

(define (exp/mul? s)
  (and (not (exp/atom? s))
       (eq? (operator s) '*)))

(define (exp/div? s)
  (and (not (exp/atom? s))
       (eq? (operator s) '/)))

(define (exp/neg? s)
  (and (not (exp/atom? s))
       (eq? (operator s) *negtive*)))

(define (exp/pon? s)
  (and (not (exp/atom? s))
       (eq? (operator s) *pos-nor-neg*)))

(define (exp/root? s)
  (and (not (exp/atom? s))
       (eq? (operator s) *root*)))

(define (exp/power? s)
  (and (not (exp/atom? s))
       (eq? (operator s) *power*)))

(define (exp/paren? s)
  (and (not (exp/atom? s))
       (eq? (operator s) *parenthsises*)))

(define (get-priority s)
  (cond
    ((exp/atom?  s) 20)
    ((exp/paren? s) 30)
    ((exp/sum? s)   1)
    ((exp/sub? s)   1)
    ((exp/mul? s)   2)
    ((exp/div? s)   3)
    ((exp/root?  s) 5)
    ((exp/power? s) 5)
    (else 3)))

(define (exp/add-paren s)
  (list *parenthsises* s))

(define (exp/peel-paren s)
  (cadr s))

(define (exp/try-unparen s)
  (if (exp/paren? s)
      (exp/peel-paren s)
      s))

; exp/paren-build
; when transform sexps into normal algebra expressions,
; we have to build parens for them.
(define (exp/paren-build s)
  (cond
    ; if the expression is quite simple we don't have
    ; to build paren for them. so just return them
    ((exp/simple-expression? s) s)
    ((exp/sum? s)
      (list '+ (exp/paren-build (exp/arg1 s))
               (exp/paren-build (exp/arg2 s))))
    (else
      (let* ((opr (exp/operator s)) (popr (get-priority s))
             (ag1 (exp/arg1 s))     (pag1 (get-priority ag1))
             (ag2 (exp/arg2 s))     (pag2 (get-priority ag2)))
        (list opr
              (if (> popr pag1)  (exp/paren-add (paren-build ag1)) (paren-build ag1))
              (if (>= popr pag2) (exp/paren-add (paren-build ag2)) (paren-build ag2)))))))

(define p paren-build)