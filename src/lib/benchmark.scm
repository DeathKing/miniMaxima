;;; Benchmark infrastructure
;;;
;;; A simple benchmark util
(load-option 'format)

(define (repeat times exp)
  (if (= 0 times)
      'done
      (begin
        (exp)
        (repeat (- times 1) exp))))
     
(define (deadloop exp)
  (exp)
  (deadloop exp))

(define (benchmark desc exp)
  (with-timings exp
    (lambda (run-time gc-time real-time)
      (let ((gc-time   (* (internal-time/ticks->seconds gc-time) 1000.0))
            (cpu-time  (* (internal-time/ticks->seconds run-time) 1000.0))
            (real-time (* (internal-time/ticks->seconds real-time) 1000.0)))
        (format #t "~@20A: ~@8A ~@8A ~@8A~%" desc run-time gc-time real-time)))))

(define-syntax bm
  (syntax-rules ()
    ((_ desc thunk)
     (benchmark desc (lambda () thunk)))))

;;; Syntax: (time <exp_thunk>)
;;; Description:
;;;   This is a Chez Scheme Flavored time benchmark function(syntax)
;;;
;;;   time will call MIT-Scheme built-in function with-timings to measure
;;;   function calls. Case time is a syntax, so you won't need a lambda to
;;;   protect your expresstion
;;;
;;; Example:
;;;   ; benchmark evalute integer 1
;;;   (time 1)
;;;
;;;   ; benchmark 100 times function calls
;;;   (time (repeat-times 100 ((lambda (x) x) 1)))
(define-syntax time
  (syntax-rules ()
    ((_ thunk)
     (with-timings
       (lambda () thunk)
       (lambda (run-time gc-time real-time)
         (let ((gc-time   (* (internal-time/ticks->seconds gc-time) 1000.0))
               (cpu-time  (* (internal-time/ticks->seconds run-time) 1000.0))
               (real-time (* (internal-time/ticks->seconds real-time) 1000.0)))
           (format #t "~@8A ms elapsed cpu time~%"  cpu-time)
           (format #t "~@8A ms elapsed real time~%" real-time)
           (format #t "~@8A ms elapsed gc time~%"   gc-time)))))))