(define (get-priority sym)
  (cond
    ((number? sym) 3)
    ((eq? sym 'parenthsises) 4)
    ((eq? sym '/) 1)
    ((s-exp? sym) (get-priority (operator sym)))
    ((or (eq? sym '+) (eq? sym '-)) 1)
    ((eq? sym '*) 2)
    (else 3)))

(define (atom? s)
  (not (pair? s)))

(define (s-exp? s)
  (list? s))

(define (arg1 s)
  (cadr s))

(define (arg2 s)
  (caddr s))

(define (operator s)
  (car s))

(define (parenthsised s-exp)
  (list 'parenthsises s-exp))

(define (parenthsislize s)
  (cond
    ((atom? s) s)
    ((and (atom? (arg1 s))
          (atom? (arg2 s)))
       s)
    (else
      (let* ((opr (operator s)) (popr (get-priority opr))
             (ag1 (arg1 s))     (pag1 (get-priority ag1))
             (ag2 (arg2 s))     (pag2 (get-priority ag2)))
        (list opr
              (if (> popr pag1) (parenthsised (parenthsislize ag1)) (parenthsislize ag1))
              (if (>= popr pag2) (parenthsised (parenthsislize ag2)) (parenthsislize ag2)))))))


(define a '(* (+ 1 2) 3))
(define b '(/ (+ (/ a
                    (* b c))
                 (/ 1 n))
              3))

(define p parenthsislize)


(define (component-width c)
  (car c))

(define (component-height c)
  (cadr c))

(define (component-drawable c)
  (caddr c))

(define (make-component s)
  (cond
    ((atom? s)  (make-atom s))
    ((sum? s)   (make-sum s))
    ((sub? s)   (make-sub s))
    ((mul? s)   (make-mul s))
    ((div? s)   (make-div s))
    ((paren? s) (make-paren s))))

(define (sum? s)   (eq? (operator s) '+))
(define (sub? s)   (eq? (operator s) '-))
(define (mul? s)   (eq? (operator s) '*))
(define (div? s)   (eq? (operator s) '/))
(define (paren? s) (eq? (operator s) 'parenthsises))

(define (make-atom s)
  (list (display-length s)
        1
        (lambda (target x y)
          (matrix/draw (object->string s)
                       target
                       x
                       y))))

(define (make-paren s)
  (let* ((sub (make-component (cadr s)))
         (width (component-width sub))
         (height (component-height sub))
         (half-height (quotient height 2)))
   (list (+ 2 width)
         height
         (lambda (target x y)
           (begin
             (matrix/draw "(" target x                 (+ y half-height))
             (matrix/draw ")" target (+ 1 x width)     (+ y half-height))
             ((component-drawable sub) target (+ x 1) y))))))



(define (make-sum s)
  (let* ((sub1 (make-component (arg1 s)))
         (sub2 (make-component (arg2 s)))
         (sub1/w (component-width sub1)) (sub1/h (component-height sub1))
         (sub2/w (component-width sub2)) (sub2/h (component-height sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (max sub1/h sub2/h))
         (half-height (quotient height 2)))
    (list width
          height
          (lambda (target x y)
            (let ((sub1/draw-y (+ y (quotient (- height sub1/h) 2)))
                  (sub2/draw-y (+ y (quotient (- height sub2/h) 2))))
              (matrix/draw "+" target (+ 1 x sub1/w) (+ y half-height))
              ((component-drawable sub1) target x              sub1/draw-y)
              ((component-drawable sub2) target (+ 3 x sub1/w) sub2/draw-y))))))
 

(define (make-sub s)
  (let* ((sub1 (make-component (arg1 s)))
         (sub2 (make-component (arg2 s)))
         (sub1/w (component-width sub1)) (sub1/h (component-height sub1))
         (sub2/w (component-width sub2)) (sub2/h (component-height sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (max sub1/h sub2/h))
         (half-height (quotient height 2)))
    (list width
          height
          (lambda (target x y)
            (let ((sub1/draw-y (+ y (quotient (- height sub1/h) 2)))
                  (sub2/draw-y (+ y (quotient (- height sub2/h) 2))))
              (matrix/draw "-" target (+ 1 x sub1/w) (+ y half-height))
              ((component-drawable sub1) target x              sub1/draw-y)
              ((component-drawable sub2) target (+ 3 x sub1/w) sub2/draw-y))))))
 

(define (make-mul s)
  (let* ((sub1 (make-component (arg1 s)))
         (sub2 (make-component (arg2 s)))
         (sub1/w (component-width sub1)) (sub1/h (component-height sub1))
         (sub2/w (component-width sub2)) (sub2/h (component-height sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (max sub1/h sub2/h))
         (half-height (quotient height 2)))
    (list width
          height
          (lambda (target x y)
            (let ((sub1/draw-y (+ y (quotient (- height sub1/h) 2)))
                  (sub2/draw-y (+ y (quotient (- height sub2/h) 2))))
              (matrix/draw "*" target (+ 1 x sub1/w) (+ y half-height))
              ((component-drawable sub1) target x              sub1/draw-y)
              ((component-drawable sub2) target (+ 3 x sub1/w) sub2/draw-y))))))
 

(define (make-div s)
  (let* ((sub1 (make-component (arg1 s)))
         (sub2 (make-component (arg2 s)))
         (sub1/w (component-width sub1)) (sub1/h (component-height sub1))
         (sub2/w (component-width sub2)) (sub2/h (component-height sub2))
         (width  (+ 2 (max sub1/w sub2/w)))
         (height (+ 1 sub1/h sub2/h)))
    (list width
          height
          (lambda (target x y)
            (let ((sub1/draw-x (+ x (quotient (- width sub1/w) 2)))
                  (sub2/draw-x (+ x (quotient (- width sub2/w) 2))))
              (matrix/draw (make-string width #\-) target x (+ y sub1/h))
              ((component-drawable sub1) target sub1/draw-x y)
              ((component-drawable sub2) target sub2/draw-x (+ 1 y sub1/h)))))))

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
  
(define screen (make-matrix 30 20))

(newline)

(define pi '(+ 1
               (/ 1
                  (+ 3
                     (/ 4
                        (+ 5
                           (/ 9
                              (+ 7
                                 (/ 16
                                    (+ 9 a))))))))))
(define sp '(/ 1
               (/ a (+ 1 2))))

(define c (make-component pi))
((component-drawable c) screen 0 0)
(matrix/display screen)
