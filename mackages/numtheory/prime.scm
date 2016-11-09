(load-relative "ptable.scm")

;;; gGCD
;;;
;;; Extend Euclidean algorithm.
;;;
;;; NOTE: *do check* the g to ensure the a b are coprime
(define (eGCD a b)
  (if (zero? a)
      (list b 0 1)
      (let ((tri (eGCD (modulo b a) a)))
        (let ((g (list-ref tri 0))
              (x (list-ref tri 1))
              (y (list-ref tri 2)))
          (list g (- y (* (quotient b a) x)) x)))))

;;; Stein
;;;
;;; Another way to fast GCD computation.
;;;
;;; TODO: No much speed efficient. see benchmark/gcd.scm
(define (Stein a b)
  (cond ((zero? a) b)
        ((zero? b) a)
        ((and (even? a) (even? b))
         (* 2 (Stein (quotient a 2) (quotient b 2))))
        ((even? a)
         (Stein (quotient a 2) b))
        ((even? b)
         (Stein a (quotient b 2)))
        (else
         (Stein (abs (- a b)) (min a b)))))

;;; Prime/Factor
;;;
;;; Factorize a integer as a series of prime numbers.
(define (Prime/Factor n)
  ;;; Inner recursive process
  (define (facrec a plist)
    (if (null? plist)
        (error "n is too large for me to factor -- " n)
        (let ((p (car plist)))
          (cond ((= a p)
                 (cons a '()))
                ((zero? (modulo a p))
                 (cons p (facrec (quotient a p) plist)))
                (else
                 (facrec a (cdr plist)))))))
  (facrec n Prime/PList))

