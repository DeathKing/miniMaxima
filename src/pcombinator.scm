(load-relative "pmatch")
(load-relative "memoize")

(define-syntax delay-parser
  (syntax-rules ()
    ((_ parser)
     (lambda args
       (apply parser args)))))

(define-syntax define-parser
  (syntax-rules ()
    ((_ parser body)
     (define parser
       (delay-parser body)))))
    
(define success
  (lambda (val rest)
    (list 'success val rest)))

(define succeed
  (memoize
    (lambda (val)
      (memoize-cps
        (lambda (str cont)
          (cont (success val str)))))))

(define failure
  (lambda (rest)
    (list 'failure rest)))

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
;(define alt
;  (memoize
;    (lambda (a b)
;      (memoize
;        (lambda (str)
;          (let ((result (a str)))
;            (pmatch result
;                    (`(success ,_ ,_) result)
;                    (`,failure (b str)))))))))
(define alt
  (memoize
    (lambda (a b)
      (memoize-cps
        (lambda (str cont)
          (a str cont)
          (b str cont))))))

;(define (bind p fn)
;  (lambda (str)
;    (pmatch (p str)
;            (`(success ,val ,rest)
;             ((fn val) rest))
;            (`,failure failure))))

(define (bind p fn)
  (lambda (str cont)
    (p str (lambda (result)
             (pmatch result
                     (`(success ,val ,rest)
                      ((fn val) rest cont))
                     (`,failure (cont failure)))))))

(define seq
  (memoize
    (lambda (a b)
      (memoize-cps
        (bind a (lambda (x)
                  (bind b (lambda (y)
                            (succeed (list x y))))))))))

;;; seq (parser, parser) -> parser
;;; p ::= a b
;(define seq
;  (memoize
;    (lambda (a b)
;      (memoize
;        (lambda (str)
;          (pmatch (a str)
;                  (`(success ,val1 ,rest1)
;                     (pmatch (b rest1)
;                             (`(success ,val2 ,rest2)
;                                (success (list val1 val2) rest2))
;                             (`,failure failure)))
;                  (`,failure failure)))))))



