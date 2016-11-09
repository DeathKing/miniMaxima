;;; MEMOIZE -- General memorization through table looking

(load-option 'format)

(define (memoize func)
  (let ((table (make-equal-hash-table))
        (id (lambda (x) x)))
    (lambda args
      (hash-table/lookup table
                         args 
                         id
                         (lambda ()
                           (let ((res (apply func args)))
                             (hash-table/put! table args res)
                             res))))))

(define (memoize-cps fn)
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
                       (`,__
                         (let ((entry (make-entry)))
                           (set! table (cons (cons str entry) table))
                           entry))))))
      (lambda (str cont)
        (let ((entry (table-ref str)))
          (pmatch entry
                  (`(() . ())
                     (push-continuation! entry cont)
                     (fn str (lambda (result)
                               (format #t "result is ~A.\n" result)
                               (format #t "Results is ~A.\n" (entry-results entry))
                               (if (result-subsumed? entry result)
                                   (for-each
                                     (lambda (cont) (cont result))
                                     (entry-continuations entry))
                                   (begin (format #t "result pushed!~%")(push-result! entry result))))))
                  (`,__
                    (push-continuation! entry cont)
                    (for-each
                      (lambda (result) (cont result))
                      (entry-results entry)))))))))

(define-syntax %:memoize!
  (syntax-rules ()
    ((_ func)
     (set! func (memoize func)))))