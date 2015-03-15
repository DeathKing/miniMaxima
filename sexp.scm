;;; SEXP.scm
;;;
;;; The middle representation of (simple) algebra expression.

(define (s-exp? s) (list? s))
(define (atom? s)  (not (pair? s)))

(define (simple-operation? s)
  (and (not (atom? s))
       (atom? (arg1 s))
       (atom? (arg2 s))))

(define (simple-expression? s)
  (or (atom? s)
      (and (atom? (arg1 s))
           (atom? (arg2 s)))))

(define (operator s) (car s))
(define (arg1 s)     (cadr s))
(define (arg2 s)     (caddr s))

(define *root*         'root)
(define *power*        'power)
(define *pos-nor-neg*  'pon)
(define *parenthsises* 'paren)

(define (sum? s)   (eq? (operator s) '+))
(define (sub? s)   (eq? (operator s) '-))
(define (mul? s)   (eq? (operator s) '*))
(define (div? s)   (eq? (operator s) '/))
(define (pon? s)   (eq? (operator s) *pos-nor-neg*))
(define (root? s)  (eq? (operator s) *root*))
(define (power? s) (eq? (operator s) *power*))
(define (paren? s)
  (and (s-exp? s)
       (eq? (operator s) *parenthsises*)))

(define (get-priority s)
  (cond
    ((atom?  s) 20)
    ((paren? s) 30)
    ((sum? s)   1)
    ((sub? s)   1)
    ((mul? s)   2)
    ((div? s)   3)
    ((root?  s) 5)
    ((power? s) 5)
    (else 3)))

(define (paren-add s-exp)  (list *parenthsises* s-exp))
(define (paren-peel s-exp) (cadr s-exp))
(define (try-unparen s)    (if (paren? s) (cadr s) s))
(define (paren-build s)
  (cond
    ((simple-expression? s) s)
    ((sum? s)
      (list '+ (paren-build (arg1 s)) (paren-build (arg2 s))))
    (else
      (let* ((opr (operator s)) (popr (get-priority s))
             (ag1 (arg1 s))     (pag1 (get-priority ag1))
             (ag2 (arg2 s))     (pag2 (get-priority ag2)))
        (list opr
              (if (> popr pag1)  (paren-add (paren-build ag1)) (paren-build ag1))
              (if (>= popr pag2) (paren-add (paren-build ag2)) (paren-build ag2)))))))

(define p paren-build)