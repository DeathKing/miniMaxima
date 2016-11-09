(load "../src/lib/benchmark.scm")
(load "../vendor/numtheory/modulo.scm")

(format #t "==============Benchmark for GCD==============~%")
(time (repeat 10000 (lambda () (gcd 18394758129038400 7219043769871263590))))

(format #t "==============Benchmark for Stein==============~%")
(time (repeat 10000 (lambda () (stein 18394758129038400 7219043769871263590))))

(if (not (= (gcd 18394758129038400 7219043769871263590)
            (stein 18394758129038400 7219043769871263590)))
    (format #t "Implementation Error!"))

(display (stein 18394758129038400 7219043769871263590))