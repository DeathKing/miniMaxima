(load "test-case.scm")
(load "screen.scm")

(load-option 'format)

(define (s-exp? s) (list? s))
(define (atom? s)  (not (pair? s)))

(define (simple-operation? s)
  (and (not (atom? s))
       (atom? (arg1 s))
       (atom? (arg2 s))))

(define (simple-expression? s)
  (or (atom? s)
      (and (atom? (arg1 s))
           (atom? (arg2 s)))))

(define (operator s) (car s))
(define (arg1 s)     (cadr s))
(define (arg2 s)     (caddr s))

(define *root*         'root)
(define *power*        'power)
(define *pos-nor-neg*  'pon)
(define *parenthsises* 'paren)

(define (sum? s)   (eq? (operator s) '+))
(define (sub? s)   (eq? (operator s) '-))
(define (mul? s)   (eq? (operator s) '*))
(define (div? s)   (eq? (operator s) '/))
(define (pon? s)   (eq? (operator s) *pos-nor-neg*))
(define (root? s)  (eq? (operator s) *root*))
(define (power? s) (eq? (operator s) *power*))
(define (paren? s)
  (and (s-exp? s)
       (eq? (operator s) *parenthsises*)))

(define (get-priority s)
  (cond
    ((atom?  s) 20)
    ((paren? s) 30)
    ((sum? s)   1)
    ((sub? s)   1)
    ((mul? s)   2)
    ((div? s)   3)
    ((root?  s) 5)
    ((power? s) 5)
    (else 3)))

(define (paren-add s-exp)  (list *parenthsises* s-exp))
(define (paren-peel s-exp) (cadr s-exp))
(define (try-unparen s)    (if (paren? s) (cadr s) s))
(define (paren-build s)
  (cond
    ((simple-expression? s) s)
    ((sum? s)
      (list '+ (paren-build (arg1 s)) (paren-build (arg2 s))))
    (else
      (let* ((opr (operator s)) (popr (get-priority s))
             (ag1 (arg1 s))     (pag1 (get-priority ag1))
             (ag2 (arg2 s))     (pag2 (get-priority ag2)))
        (list opr
              (if (> popr pag1)  (paren-add (paren-build ag1)) (paren-build ag1))
              (if (>= popr pag2) (paren-add (paren-build ag2)) (paren-build ag2)))))))

(define p paren-build)

(define (box/width c)    (list-ref c 0))
(define (box/height c)   (list-ref c 1))
(define (box/baseline c) (list-ref c 2))
(define (box/drawable c) (list-ref c 3))

(define (make-box s)
  (cond
    ((atom? s)  (make-atom s))
    ((sum? s)   (make-sum s))
    ((sub? s)   (make-sub s))
    ((mul? s)   (make-mul s))
    ((div? s)   (make-div s))
    ((root? s)  (make-root s))
    ((power? s) (make-power s))
    ((paren? s) (make-paren s))))

(define (make-atom s)
  (list (display-length s)
        1
        0
        (lambda (target x y)
          (matrix/draw (object->string s)
                       target
                       x
                       y))))

(define (make-root s)
  (let* ((inner (make-box (arg1 s)))
         (inner/w (box/width inner))
         (inner/h (box/height inner)))
    (let* ((width (+ 2 inner/w inner/h))
           (height (+ 1 inner/h))
           (baseline (quotient height 2)))
      (list width
            height
            baseline
            (lambda (target x y)
              (matrix/draw-horizon-line target (+ x inner/h) y (+ 2 inner/w) "_")
              (matrix/draw-dialog-line target "/" (+ -1 x inner/h) (+ 1 y) - + inner/h)
              (matrix/put! "√" target x (+ y inner/h))
              ((box/drawable inner) target (+ 1 x inner/h) (+ y 1)))))))

(define (make-power s)
  (let* ((base (make-box (arg1 s)))
         (exp (make-box (arg2 s)))
         (base/w (box/width base)) (base/h (box/height base)) (base/b (box/baseline base))
         (exp/w  (box/width exp))  (exp/h (box/height exp))   (exp/b (box/baseline exp)))
    (let* ((width (+ 1 (box/width base) (box/width exp)))
           (height (+ 1 (box/baseline exp) (max base/h (- exp/h exp/b))))
           (baseline (+ base/b exp/b)))
      (list width
            height
            baseline
            (lambda (target x y)
              ((box/drawable base) target x (+ y exp/b))
              ((box/drawable exp)  target (+ x base/w) y))))))

(define (make-paren s)
  (let* ((sub (make-box (paren-peel s)))
         (width (box/width sub))
         (height (box/height sub))
         (baseline (box/baseline sub)))
    (list (+ 2 width)
          height
          baseline
          (lambda (target x y)
            (matrix/draw "(" target x                 (+ y baseline))
            (matrix/draw ")" target (+ 1 x width)     (+ y baseline))
            ((box/drawable sub) target (+ x 1) y)))))

(define (make-sum s)
  (let* ((sub1 (make-box (arg1 s))) (sub2 (make-box (arg2 s)))
         (sub1/w (box/width sub1)) (sub1/h (box/height sub1)) (sub1/b (box/baseline sub1))
         (sub2/w (box/width sub2)) (sub2/h (box/height sub2)) (sub2/b (box/baseline sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (+ (max sub1/b sub2/b) (max (- sub1/h sub1/b) (- sub2/h sub2/b))))
         (baseline (max sub1/b sub2/b)))
    (list width
          height
          baseline
          (lambda (target x y)
            (let ((sub1/draw-y (+ y (- baseline sub1/b)))
                  (sub2/draw-y (+ y (- baseline sub2/b))))
              (matrix/draw "+" target (+ 1 x sub1/w) (+ y baseline))
              ((box/drawable sub1) target x              sub1/draw-y)
              ((box/drawable sub2) target (+ 3 x sub1/w) sub2/draw-y))))))

(define (make-sub s)
  (let* ((sub1 (make-box (arg1 s))) (sub2 (make-box (arg2 s)))
         (sub1/w (box/width sub1)) (sub1/h (box/height sub1)) (sub1/b (box/baseline sub1))
         (sub2/w (box/width sub2)) (sub2/h (box/height sub2)) (sub2/b (box/baseline sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (+ (max sub1/b sub2/b) (max (- sub1/h sub1/b) (- sub2/h sub2/b))))
         (baseline (max sub1/b sub2/b)))
    (list width
          height
          baseline
          (lambda (target x y)
            (let ((sub1/draw-y (+ y (- baseline sub1/b)))
                  (sub2/draw-y (+ y (- baseline sub2/b))))
              (matrix/draw "-" target (+ 1 x sub1/w) (+ y baseline))
              ((box/drawable sub1) target x              sub1/draw-y)
              ((box/drawable sub2) target (+ 3 x sub1/w) sub2/draw-y))))))

(define (make-mul s)
  (let* ((sub1 (make-box (arg1 s))) (sub2 (make-box (arg2 s)))
         (sub1/w (box/width sub1)) (sub1/h (box/height sub1)) (sub1/b (box/baseline sub1))
         (sub2/w (box/width sub2)) (sub2/h (box/height sub2)) (sub2/b (box/baseline sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (+ (max sub1/b sub2/b) (max (- sub1/h sub1/b) (- sub2/h sub2/b))))
         (baseline (max sub1/b sub2/b)))
    (list width
          height
          baseline
          (lambda (target x y)
            (let ((sub1/draw-y (+ y (- baseline sub1/b)))
                  (sub2/draw-y (+ y (- baseline sub2/b))))
              (matrix/draw "*" target (+ 1 x sub1/w) (+ y baseline))
              ((box/drawable sub1) target x              sub1/draw-y)
              ((box/drawable sub2) target (+ 3 x sub1/w) sub2/draw-y))))))

(define (make-div s)
  (let* ((sub1 (make-box (try-unparen (arg1 s))))
         (sub2 (make-box (try-unparen (arg2 s))))
         (sub1/w (box/width sub1)) (sub1/h (box/height sub1))
         (sub2/w (box/width sub2)) (sub2/h (box/height sub2))
         (width  (+ 2 (max sub1/w sub2/w)))
         (height (+ 1 sub1/h sub2/h))
         (baseline (box/height sub1)))
    (list width
          height
          baseline
          (lambda (target x y)
            (let ((sub1/draw-x (+ x (quotient (- width sub1/w) 2)))
                  (sub2/draw-x (+ x (quotient (- width sub2/w) 2))))
              (matrix/draw (make-string width #\-) target x (+ y baseline))
              ((box/drawable sub1) target sub1/draw-x y)
              ((box/drawable sub2) target sub2/draw-x (+ 1 y baseline)))))))

(define (make-boxed-formula s margin-horizon margin-vertival)
  (let ((formula (make-box s)))
    (let ((width    (+ 2 (box/width formula) (* 2 margin-horizon)))
          (height   (+ 2 (box/height formula) (* 2 margin-vertival)))
          (baseline (+ 1 (box/baseline formula) margin-vertival)))
      (list width
            height
            baseline
            (lambda (target x y)
              (matrix/draw-box target x y width height)
              ((box/drawable formula) target
                                      (+ 1 x margin-horizon)
                                      (+ 1 y margin-vertival)))))))

(define screen (make-matrix 90 20))

;(matrix/draw-box screen 0 0 30 10)
(newline)
;  '(root (/ (+ a (+ (root (/ pi 2)) c)) 2))))
(define c  (make-box t2))
((box/drawable c) screen 0 0)
(matrix/display screen)