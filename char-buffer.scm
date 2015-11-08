; char-buffer.scm

(define (make-matrix x y)
  (make-initialized-vector y
    (lambda (k)
      (make-vector x '()))))

(define (matrix/put! s m x y)
  (vector-set! (vector-ref m y) x s))

(define (matrix/draw s m x y)
  (if (string-null? s)
    '()
    (begin
      (matrix/put! (string-head s 1) m x y)
      (matrix/draw (string-tail s 1) m (+ x 1) y))))

(define (matrix/draw-dialog-line m x y px py c s)
  (if (= c 0)
    '()
    (begin
      (matrix/put! s m x y)
      (matrix/draw-dialog-line m (px x 1) (py y 1) px py (- c 1) s))))

(define (vector/println v)
  (vector-map
    (lambda (x)
      (if (null? x) (display " ") (display x)))
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

(define (matrix/draw-vertical-line m x y h . l)
  (if (= h 1)
    '()
    (let ((l (if (null? l) "|" (car l))))
      (matrix/put! l m x y)
      (matrix/draw-vertical-line m x (+ y 1) (- h 1) l))))

(define (matrix/draw-horizon-line m x y w . l)
  (if (= w 1)
    '()
    (let ((l (if (null? l) "-" (car l))))
      (matrix/put! l m x y)
      (matrix/draw-horizon-line m (+ x 1) y (- w 1) l))))

(define (matrix/draw-box m x y w h)
  (matrix/draw-vertical-line m x y h)
  (matrix/draw-vertical-line m (+ x w) y h)
  (matrix/draw-horizon-line m x y w)
  (matrix/draw-horizon-line m x (+ -1 y h) w)
  (matrix/draw-point m x y)
  (matrix/draw-point m x (+ -1 y h))
  (matrix/draw-point m (+ x w) y)
  (matrix/draw-point m (+ x w) (+ -1 y h)))
