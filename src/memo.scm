;;; MEMO -- General memorization through table looking

(define (memoize func)
  (let ((table (make-equal-hash-table)))
    (lambda arg
      (hash-table/lookup table arg 
        (lambda (x) x)
        (lambda ()
          (let ((res (apply func arg)))
            (hash-table/put! table arg res)
            res))))))

(define-syntax %:memoize!
  (syntax-rules ()
    ((_ func)
     (set! func (memoize func)))))