(define-syntax assert-equal
  (syntax-rules ()
    ((_ thunk1 thunk2)
     (if (equal? thunk1 thunk2)
         'nothing
         (error "Assert not equal")))))