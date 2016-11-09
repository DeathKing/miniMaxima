(load-relative "../mackages/numtheory/prime.scm")

(load-relative "../src/vendor/test-manager/load.scm")

; This is a test group named simple-stuff
(in-test-group
 simple-stuff

 ; This is one test named arithmetic
 (define-test (factor)
   "Checking that set! and arithmetic work"
   (check (equal? (Prime/Factor 100) '(2 2 5 5)))
   (check (equal? (Prime/Factor 5231) '(5231))))
 
 (define-test (egcd)
    "Checking that egcd algorithm is good"
    (let* ((triple (egcd 5 7))
           (g (list-ref triple 0))
           (x (list-ref triple 1))
           (y (list-ref triple 2)))
      (check (= g 1))
      (check (= (+ (* 5 x) (* 7 y)) 1)))))

(run-registered-tests)

 ;; Each of these will become a separate anonymous one-form test
 ;(define-each-test
 ;  (check (= 4 (+ 2 2)) "Two and two should make four.")
 ;  (check (= 2147483648 (+ 2147483647 1)) "Addition shouldn't overflow."))

 ;; Each of these will become a separate anonymous one-form test using check
 ;(define-each-check
 ;  (= 6 (+ 2 2 2))
 ;  (equal? '(1 2 3) (cons 1 '(2 3))))

 ;; This is a test that looks like a REPL interaction
 ;(define-test (interactive)
 ;  (interaction
 ;   (define foo 5)
 ;   foo
 ;   (produces 5)  ; This compares against the value of the last form
 ;   (set! foo 6)
 ;   (* foo foo)
 ;   (produces 36))))