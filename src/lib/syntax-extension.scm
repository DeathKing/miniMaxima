;(define-syntax q-guard
;  (syntax-rules (_)
;    ((_ q default)
;     (if (null? q) default q))
;    (else
;      (error "ill-formed syntax!"))))

(define qval-guard
  (lambda (val default)
    (if (null? val) default val)))

(define (assoc-default key lst default)
  (let ((found (assoc key lst)))
    (if (false? found)
        default
        found)))

(define (assoc-defaultp key lst procedure)
  (let ((found (assoc key lst)))
    (if (false? found)
        (procedure key)
        found)))