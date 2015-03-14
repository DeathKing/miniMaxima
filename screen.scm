(define (make-matrix x y)
  (make-initialized-vector y
    (lambda (k) (make-vector x '()))))

(define (matrix/draw s m x y)
  (if (string-null? s)
    '()
    (begin
      (matrix/put! (string-head s 1) m x y)
      (matrix/draw (string-tail s 1) m (+ x 1) y))))

(define (matrix/put! s m x y)
  (vector-set! (vector-ref m y) x s))

(define (vector/println v)
  (vector-map
    (lambda (x)
      (if (null? x)
        (display " ")
        (display x)))
    v)
  (newline))

(define (matrix/display m)
  (vector-map vector/println m))

(define (object->string o)
  (cond
    ((symbol? o) (symbol->string o))
    ((number? o) (number->string o))))

(define (display-length o)
  (string-length (object->string o)))

(define (matrix/draw-point m x y . p)
  (if (null? p)
    (matrix/put! "+" m x y)
    (matrix/put! (string-head (car p) 0) m x y)))

(define (matrix/draw-vertical-line m x y h)
  (if (= h 1)
    '()
    (begin
      (matrix/put! "|" m x y)
      (matrix/draw-vertical-line m x (+ y 1) (- h 1)))))

(define (matrix/draw-horizon-line m x y w)
  (if (= w 1)
    '()
    (begin
      (matrix/put! "-" m x y)
      (matrix/draw-horizon-line m (+ x 1) y (- w 1)))))

(define (matrix/draw-box m x y w h)
  (matrix/draw-vertical-line m x y h)
  (matrix/draw-vertical-line m (+ x w) y h)
  (matrix/draw-horizon-line m x y w)
  (matrix/draw-horizon-line m x (+ -1 y h) w)
  (matrix/draw-point m x y)
  (matrix/draw-point m x (+ -1 y h))
  (matrix/draw-point m (+ x w) y)
  (matrix/draw-point m (+ x w) (+ -1 y h)))