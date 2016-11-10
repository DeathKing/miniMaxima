(use-mackage 'xybuffer)

(define *gBoxTable* '())

(define (InstallBoxProcedureGlobal! tag pro)
  (set! *gBoxTable* (cons (cons tag pro) *gBoxTable*)))

(define  Box/New         list)

(define (Box/Width b)    (list-ref b 0))
(define (Box/Height b)   (list-ref b 1))
(define (Box/Baseline b) (list-ref b 2))

;;; Drawable int -> int -> XYBuffer -> void
(define (Box/Drawable b) (list-ref b 3))

(define (Box/Build expr)
  (let* ((tag (TagExpr/Tag expr))
         (proc (assoc tag *gBoxTable*)))
    (if (false? proc)
        (error "Proceduer not regist in global table for tag -- " tag)
        (proc expr))))

(define (Box/BuildAtom expr)
  (Box/New (display-length expr) ; width
           1                     ; height
           0                     ; baseline
           (lambda (x y target)
             (XYBuffer/MVDrawString (object->string expr)
                                    x
                                    y
                                    target))))

(define (Box/BuildAdd expr)
  (let* ((sub1 (Box/Build (AddExpr/LHS s)))
         (sub2 (Box/Build (AddExpr/RHS s)))
         (sub1/w (Box/Width sub1)) (sub1/h (Box/Height sub1)) (sub1/b (Box/Baseline sub1))
         (sub2/w (Box/Width sub2)) (sub2/h (Box/Height sub2)) (sub2/b (Box/Baseline sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (+ (max sub1/b sub2/b) (max (- sub1/h sub1/b) (- sub2/h sub2/b))))
         (baseline (max sub1/b sub2/b)))
    (Box/New width
             height
             baseline
             (lambda (x y target)
               (let ((sub1/draw-y (+ y (- baseline sub1/b)))
                     (sub2/draw-y (+ y (- baseline sub2/b))))
                 (XYBuffer/MVDrawChar #\+ (+ 1 x sub1/w) (+ y baseline) target)
                 ((Box/Drawable sub1) x              sub1/draw-y target)
                 ((Box/Drawable sub2) (+ 3 x sub1/w) sub2/draw-y target))))))

(define (Box/BuildSub expr)
  (let* ((sub1 (Box/Build (SubExpr/LHS expr)))
         (sub2 (Box/Build (SubExpr/RHS expr)))
         (sub1/w (Box/Width sub1)) (sub1/h (Box/Height sub1)) (sub1/b (Box/Baseline sub1))
         (sub2/w (Box/Width sub2)) (sub2/h (Box/Height sub2)) (sub2/b (Box/Baseline sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (+ (max sub1/b sub2/b) (max (- sub1/h sub1/b) (- sub2/h sub2/b))))
         (baseline (max sub1/b sub2/b)))
    (Box/New width
             height
             baseline
             (lambda (x y target)
               (let ((sub1/draw-y (+ y (- baseline sub1/b)))
                     (sub2/draw-y (+ y (- baseline sub2/b))))
                 (XYBuffer/MVDrawChar #\- (+ 1 x sub1/w) (+ y baseline) target)
                 ((Box/Drawable sub1) x              sub1/draw-y target)
                 ((Box/Drawable sub2) (+ 3 x sub1/w) sub2/draw-y target))))))

(define (Box/BuildMul expr)
  (let* ((sub1 (Box/Build (SubExpr/LHS expr)))
         (sub2 (Box/Build (SubExpr/LHS expr)))
         (sub1/w (Box/Width sub1)) (sub1/h (Box/Height sub1)) (sub1/b (Box/Baseline sub1))
         (sub2/w (Box/Width sub2)) (sub2/h (Box/Height sub2)) (sub2/b (Box/Baseline sub2))
         (width (+ 3 sub1/w sub2/w))
         (height (+ (max sub1/b sub2/b) (max (- sub1/h sub1/b) (- sub2/h sub2/b))))
         (baseline (max sub1/b sub2/b)))
    (Box/New width
             height
             baseline
             (lambda (x y target)
               (let ((sub1/draw-y (+ y (- baseline sub1/b)))
                     (sub2/draw-y (+ y (- baseline sub2/b))))
                 (XYBuffer/MVDrawChar #\* (+ 1 x sub1/w) (+ y baseline) target)
                 ((box/drawable sub1) x              sub1/draw-y target)
                 ((box/drawable sub2) (+ 3 x sub1/w) sub2/draw-y target))))))

(define (Box/BuildDiv expr)
  (let* ((sub1 (Box/Build (DivExpr/LHS expr)));(AlgeExpr/TryUnparen (DivExpr/LHS s))))
         (sub2 (Box/Build (DivExpr/RHS expr)));(AlgeExpr/TryUnparen (DivExpr/RHS s))))
         (sub1/w (Box/Width sub1)) (sub1/h (Box/Height sub1))
         (sub2/w (Box/Width sub2)) (sub2/h (Box/Height sub2))
         (width  (+ 2 (max sub1/w sub2/w)))
         (height (+ 1 sub1/h sub2/h))
         (baseline (Box/Height sub1)))
    (Box/New width
             height
             baseline
             (lambda (x y target)
               (let ((sub1/draw-x (+ x (quotient (- width sub1/w) 2)))
                     (sub2/draw-x (+ x (quotient (- width sub2/w) 2))))
                 (XYBuffer/MVDrawString (make-string width #\-) x (+ y baseline) target)
                 ((Box/Drawable sub1) sub1/draw-x y target)
                 ((Box/Drawable sub2) sub2/draw-x (+ 1 y baseline) target))))))

;(define (Box/BuildRoot expr)
;  (let* ((inner (Box/Build (RootExpr/Power expr)))
;         (inner/w (Box/Width inner))
;         (inner/h (Box/Height inner)))
;    (let* ((width (+ 2 inner/w inner/h))
;           (height (+ 1 inner/h))
;           (baseline (quotient height 2)))
;      (Box/New width
;                  height
;                  baseline
;                  (lambda (x y target)
;                    (matrix/draw-horizon-line target (+ x inner/h)    y (+ 2 inner/w) "_")
;                    (matrix/draw-dialog-line  target (+ -1 x inner/h) (+ 1 y) - + inner/h "/")
;                    (matrix/put! "√"          target x                (+ y inner/h))
;                    ((box/drawable inner)     target (+ 1 x inner/h)  (+ y 1)))))))

(define (Box/BuildExpt expr)
  (let* ((base (Box/Build (ExptExpr/Base expr)))
         (exp  (Box/Build (ExptExpr/Exponent expr)))
         (base/w (Box/Width base)) (base/h (Box/Height base)) (base/b (Box/Baseline base))
         (exp/w  (Box/Width exp))  (exp/h  (Box/Height exp))  (exp/b  (Box/Baseline exp)))
    (let* ((width (+ base/w exp/w))
           (height (+ 1 (Box/Baseline exp) (max base/h (- exp/h exp/b))))
           (baseline (+ base/b exp/b)))
      (box/create width
                  height
                  baseline
                  (lambda (x y target)
                    ((Box/Drawable base) x (+ y exp/b)  target )
                    ((Box/Drawable exp)  (+ x base/w) y target ))))))

(InstallBoxProcedureGlobal! 'add  Box/BuildAdd)
(InstallBoxProcedureGlobal! 'sub  Box/BuildSub)
(InstallBoxProcedureGlobal! 'mul  Box/BuildMul)
(InstallBoxProcedureGlobal! 'div  Box/BuildDiv)
(InstallBoxProcedureGlobal! 'expt Box/BuildExpt)
;(InstallBoxProcedureGlobal! 'root Box/BuildRoot)