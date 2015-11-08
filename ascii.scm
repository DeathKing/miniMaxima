(load "sexp.scm")
(load "test-case.scm")
(load "screen.scm")

(load-option 'format)

; Constructor and Selectors
(define  box/create       list)
(define (box/width c)    (list-ref c 0))
(define (box/height c)   (list-ref c 1))
(define (box/baseline c) (list-ref c 2))
(define (box/drawable c) (list-ref c 3))

; make-box :: EXP -> BOX
(define (make-box s)
  (cond
    ((atom? s)  (make-box/atom s))
    ((neg? s)   (make-box/neg  s))
    ((sum? s)   (make-box/sum s))
    ((sub? s)   (make-box/sub s))
    ((mul? s)   (make-box/mul s))
    ((div? s)   (make-box/div s))
    ((root? s)  (make-box/root s))
    ((power? s) (make-box/power s))
    ((paren? s) (make-box/paren s))
    (else
      (error "Unknon type of expression -- " s))))

; Atom is simple enough so that we can build it easily
(define (make-box/atom s)
  (box/create (display-length s)
              1
              0
              (lambda (target x y)
                (matrix/draw (object->string s)
                             target
                             x
                             y))))

(define (make-box/neg s)
  (let* ((inner (make-box (arg1 s)))
         (inner/w (box/width inner))
         (inner/h (box/height inner)))
    ))


(define (make-box/root s)
  (let* ((inner (make-box (exp/arg1 s)))
         (inner/w (box/width inner))
         (inner/h (box/height inner)))
    (let* ((width (+ 2 inner/w inner/h))
           (height (+ 1 inner/h))
           (baseline (quotient height 2)))
      (box/create width
                  height
                  baseline
                  (lambda (target x y)
                    (matrix/draw-horizon-line target (+ x inner/h)    y (+ 2 inner/w) "_")
                    (matrix/draw-dialog-line  target (+ -1 x inner/h) (+ 1 y) - + inner/h "/")
                    (matrix/put! "√"          target x                (+ y inner/h))
                    ((box/drawable inner)     target (+ 1 x inner/h)  (+ y 1)))))))

(define (make-box/power s)
  (let* ((base (make-box (exp/arg1 s)))
         (exp  (make-box (exp/arg2 s)))
         (base/w (box/width base)) (base/h (box/height base)) (base/b (box/baseline base))
         (exp/w  (box/width exp))  (exp/h (box/height exp))   (exp/b (box/baseline exp)))
    (let* ((width (+ base/w exp/w))
           (height (+ 1 (box/baseline exp) (max base/h (- exp/h exp/b))))
           (baseline (+ base/b exp/b)))
      (box/create width
                  height
                  baseline
                  (lambda (target x y)
                    ((box/drawable base) target x (+ y exp/b))
                    ((box/drawable exp)  target (+ x base/w) y))))))

(define (make-box/paren s)
  (let* ((sub (make-box (exp/peel-paren s)))
         (width (box/width sub))
         (height (box/height sub))
         (baseline (box/baseline sub)))
    (box/create (+ 2 width)
                height
                baseline
                (lambda (target x y)
                  (matrix/draw "(" target x             (+ y baseline))
                  (matrix/draw ")" target (+ 1 x width) (+ y baseline))
                  ((box/drawable sub) target (+ x 1) y)))))

(define (make-box/sum s)
  (let* ((sub1 (make-box (exp/arg1 s)))
         (sub2 (make-box (exp/arg2 s)))
         (sub1/w (box/width sub1)) (sub1/h (box/height sub1)) (sub1/b (box/baseline sub1))
         (sub2/w (box/width sub2)) (sub2/h (box/height sub2)) (sub2/b (box/baseline sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (+ (max sub1/b sub2/b) (max (- sub1/h sub1/b) (- sub2/h sub2/b))))
         (baseline (max sub1/b sub2/b)))
    (box/create width
                height
                baseline
                (lambda (target x y)
                  (let ((sub1/draw-y (+ y (- baseline sub1/b)))
                        (sub2/draw-y (+ y (- baseline sub2/b))))
                    (matrix/draw "+" target (+ 1 x sub1/w) (+ y baseline))
                    ((box/drawable sub1) target x              sub1/draw-y)
                    ((box/drawable sub2) target (+ 3 x sub1/w) sub2/draw-y))))))

(define (make-box/sub s)
  (let* ((sub1 (make-box (exp/arg1 s)))
         (sub2 (make-box (exp/arg2 s)))
         (sub1/w (box/width sub1)) (sub1/h (box/height sub1)) (sub1/b (box/baseline sub1))
         (sub2/w (box/width sub2)) (sub2/h (box/height sub2)) (sub2/b (box/baseline sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (+ (max sub1/b sub2/b) (max (- sub1/h sub1/b) (- sub2/h sub2/b))))
         (baseline (max sub1/b sub2/b)))
    (box/create width
                height
                baseline
                (lambda (target x y)
                  (let ((sub1/draw-y (+ y (- baseline sub1/b)))
                        (sub2/draw-y (+ y (- baseline sub2/b))))
                    (matrix/draw "-" target (+ 1 x sub1/w) (+ y baseline))
                    ((box/drawable sub1) target x              sub1/draw-y)
                    ((box/drawable sub2) target (+ 3 x sub1/w) sub2/draw-y))))))

(define (make-box/mul s)
  (let* ((sub1 (make-box (arg1 s))) (sub2 (make-box (arg2 s)))
         (sub1/w (box/width sub1)) (sub1/h (box/height sub1)) (sub1/b (box/baseline sub1))
         (sub2/w (box/width sub2)) (sub2/h (box/height sub2)) (sub2/b (box/baseline sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (+ (max sub1/b sub2/b) (max (- sub1/h sub1/b) (- sub2/h sub2/b))))
         (baseline (max sub1/b sub2/b)))
    (box/create width
          height
          baseline
          (lambda (target x y)
            (let ((sub1/draw-y (+ y (- baseline sub1/b)))
                  (sub2/draw-y (+ y (- baseline sub2/b))))
              (matrix/draw "*" target (+ 1 x sub1/w) (+ y baseline))
              ((box/drawable sub1) target x              sub1/draw-y)
              ((box/drawable sub2) target (+ 3 x sub1/w) sub2/draw-y))))))

(define (make-box/div s)
  (let* ((sub1 (make-box (try-unparen (arg1 s))))
         (sub2 (make-box (try-unparen (arg2 s))))
         (sub1/w (box/width sub1)) (sub1/h (box/height sub1))
         (sub2/w (box/width sub2)) (sub2/h (box/height sub2))
         (width  (+ 2 (max sub1/w sub2/w)))
         (height (+ 1 sub1/h sub2/h))
         (baseline (box/height sub1)))
    (box/create width
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
      (box/create width
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
(define c  (make-box (p t3)))
((box/drawable c) screen 0 0)
(matrix/display screen)