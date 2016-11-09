(load-relative "../src/lib/benchmark.scm")

(define (FastExpt b n)
  (cond ((= n 0) 1)
        ((even? n) (let ((x (FastExpt b (/ n 2)))) (* x x)))
        (else (* b (FastExpt b (- n 1))))))

