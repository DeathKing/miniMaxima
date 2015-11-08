(define (load-relative filename)
  (with-working-directory-pathname 
    (directory-namestring (current-load-pathname))
    (lambda () (load filename))))