;;; Env.scm
;;;
;;; Nameless and typeless environment infrastructure. By nameless, it means
;;; there is no way to reference a env by some string or symbol like value.
;;; By typeless it means the env is basically a list structure and we never
;;; even put a tag on it.

;;; Make a new (empty) env.
(define (Env/New)
  '())

(define Env/MakeFromEntries
  list)

;;; p - Env/Empty?
;;; @env the environment.
;;;
(define (Env/Empty? env)
  (null? env))

(define (Env/Extend var val env)
  (cons (cons var val) env))

(define (Env/InsertEntry entry env)
  (cons entry env))

(define (Env/Lookup var env)
  (let ((entry (assoc var env)))
    (if (false? entry)
        (error "No such variable in env -- " var)
        (cdr entry))))

(define (Env/LookupDefault var default env)
  (let ((entry (assoc var env)))
    (if (false? entry)
        default
        (cdr entry))))

(define (Env/LookupDefaultP var defaultp env)
  (let ((entry (assoc var env)))
    (if (false? entry)
        (defaultp var env)
        (cdr entry))))

(define (Env/Merge env1 env2)
  (append env1 env2))

(define (Env/Inspect env)
  (if (null? env)
      'done
      (let ((entry (car env)))
        (format #t "~A => ~A ~%" (car entry) (cdr entry))
        (Env/Inspect (cdr env)))))
