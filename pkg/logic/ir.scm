;;; intermediate language of logic
;;; Logic should be call-by-name languages


(namespace 'logic

  (define (negative p)
    (list 'negative p))

  (define (impliy p q)
    (list 'impliy p q))

  (define (conjunct p q)
    (list 'conjunct p q))

  (define (disconjunct p q)
    (list 'disconjunct p q))
  
  (define (double-nagative p)
    (negative (negative p)))


  (define (same-syntactic-object p q)
    (eq? p q)))
