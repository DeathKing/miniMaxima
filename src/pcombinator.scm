; pcombinator.scm -- A simple parser combinator
;
; ref: https://github.com/epsil/gll

(load-relative "pmatch")
(load-relative "memoize")

(define-syntax delay-parser
  (syntax-rules ()
    ((_ parser)
     (lambda args (apply parser args)))))

(define (run-parser parser str)
  (let ((results '()))
    (parser str (lambda (result)
                  (pmatch result
                          (`(success ,__ ,__)
                             (format #t "Success matched!~%")
                             (set! results (cons result results)))
                          (`,failure
                            (format #t "Miss matched!~%")
                            failure))))
    results))

(define (make-parser parser)
  (lambda (str  cont)
    (if cont
        (parser str cont)
        (run-parser parser str))))

; SYNTAX
; (define-parser parser-name body)
(define-syntax define-parser
  (syntax-rules ()
    ((_ parser body)
     (define parser (delay-parser body)))))

; (define-syntax define-parser
;   (syntax-rules ()
;     ((_ parser body)
;      (define parser (make-parser (delay-parser body))))))

; DATA-STRUCTURE    
(define success
  (lambda (val rest)
    (list 'success val rest)))

; DATA-CONSTRUCTOR
(define succeed
  (memoize
    (lambda (val)
      (memoize-cps
        (lambda (str cont)
          (cont (success val str)))))))

; DATA-STRUCTURE
(define failure
  (lambda (rest)
    (list 'failure rest)))

; PRIMITIVE-PARSER
(define match-string
  (memoize
    (lambda (pattern)
      (memoize-cps
        (lambda (str cont)
          (let ((len (min (string-length str) (string-length pattern))))
            (let ((head (string-head str len))
                  (tail (string-tail str len)))
              (if (equal? head pattern)
                  (cont (success head tail))
                  (cont (failure str))))))))))

;;; alt (parser, parser) -> parser
;;; p ::= a | b
(define alt
  (memoize
    (lambda (a b)
      (memoize-cps
        (lambda (str cont)
          (a str cont)
          (b str cont))))))

(define (bind p fn)
  (lambda (str cont)
    (p str (lambda (result)
             (pmatch result
                     (`(success ,val ,rest) ((fn val) rest cont))
                     (`,failure (cont failure)))))))

(define seq
  (memoize
    (lambda (a b)
      (memoize-cps
        (bind a (lambda (x)
                  (bind b (lambda (y)
                            (succeed (list x y))))))))))

(define-parser p
               (alt (seq p (match-string "a"))
                    (match-string "a")))

(define cont (lambda (k) k))
