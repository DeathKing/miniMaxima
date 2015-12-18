;;; MEMOIZE -- General memorization through table looking

(load-option 'format)

(define (memoize func)
  (let ((table (make-equal-hash-table)))
    (lambda args
      (hash-table/lookup table args 
        (lambda (x)
          (format #t "Cache hit for args is ~A.~%" x)
          x)
        (lambda ()
          (format #t "Cache missing for args is ~A.~%" args)
          (let ((res (apply func args)))
            (hash-table/put! table args res)
            res))))))

(define (memoize-cps func)
  (let ((table (list)))
    (let* ((entry-continuations car)
           (entry-results cdr)
           (push-continuation!
             (lambda (entry cont)
               (set-car! entry (cons cont (entry-continuations entry)))))
           (push-result!
             (lambda (entry result)
               (set-cdr! entry (cons result (entry-results entry)))))
           (result-subsumed?
             (lambda (entry result)
               (member result (entry-results entry))))
           (make-entry
             (lambda ()
               (cons (list) (list))))
           (table-ref
             (lambda (str)
               (pmatch (assoc str table)
                       (`(,str . ,entry) entry)
                       (`,_
                        (let ((entry make-entry))
                          (set! table (cons (cons str entry) table))
                          entry))))))
      (lambda (str cont)
        (let ((entry (table-ref str)))
          (pmatch entry
                  (`('() . '())
                   (push-continuation! entry cont)
                   (fn str (lambda (result)
                             (if (result-subsumed? entry result)
                                 (for ((cont (entry-continuations entry)))
                                      (cont result))
                                 (push-result! entry result)))))
                  (`,_
                   (push-continuation! entry cont)
                   (for ((resutle (entry-results entry)))
                        (cont result)))))))))

(define-syntax %:memoize!
  (syntax-rules ()
    ((_ func)
     (set! func (memoize func)))))