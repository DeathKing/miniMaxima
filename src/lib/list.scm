;;; Scheme Language Syntax/Library Extension

(define (list-unique lst)
  (if (null? lst)
      '()
      (let ((first (car lst)) (rest (cdr lst)))
        (if (member first rest)
            (list-unique rest)
            (cons first (list-unique rest))))))

