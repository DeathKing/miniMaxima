(use-mackage 'arith)

(load-relative "../src/vendor/test-manager/load.scm")

;;; prepare term and polynomial to test


;;; prepare test case
(in-test-group
 simple-stuff

 ; This is one test named arithmetic
 (define-test (sexpr-to-expr)
   "Checking that set! and arithmetic work"
   (define t1 (TagExpr/SexprToExpr '(+ 1 (* 3 4))))
   (define t2 (TagExpr/SexprToExpr '(+ (* 2 x) 3)))
   (define t3 (TagExpr/SexprToExpr '(- (* 2 (^ x 2)) 4)))
   
   (check (equal? t1 '(add 1 (mul 3 4))))
   (check (equal? t2 '(add (mul 2 x) 3)))
   (check (equal? t3 '(sub (mul 2 (expt x 2)) 4)))))

(run-registered-tests)