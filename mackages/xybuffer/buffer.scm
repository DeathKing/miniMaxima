(define (XYBuffer/GetConsoleWidth))

(define XYBuffer/*DefaultHeight* 20)

(define (XYBuffer/New width . height)
  ;;; width must be fixed
  ;;; height is adjustable if neccessary
  (let ((height (q-guard height XYBuffer/*DefaultHeight*)))
    (list 'XYBuffer
          (list (cons 'width  width)
                (cons 'height height)
                (cons 'curx   0)
                (cons 'cury   0))
          (make-initialized-vector height
                                   (lambda (width) (make-vector x #\space))))))

(define (XYBuffer/GetAttr attr buffer)
  (let ((entry (assoc attr (cadr buffer))))
    (if entry
        (car entry)
        (error "No such attr for buffer -- " attr))))

(define (XYBuffer/GetAttrDefault attr default buffer)
  (let ((entry (assoc attr (cadr buffer))))
    (if entry
        (car entry)
        default)))

(define (XYBuffer/SetAttr attr value buffer)
  (let ((entry (assoc attr (cadr buffer))))
    (if entry
        (set-cdr! entry value)
        (error "No such attr for buffer to set -- " attr))))

(define (XYBuffer/ExtendHeight delta buffer))

(define (XYBuffer/Buffer buffer)   (list-ref buffer 2))
(define (XYBuffer/PutChar x y char buffer)
  (vector-set! (vector-ref (XYBuffer/Buffer buffer) y) x char))

(define (XYBuffer/Width buffer)    (XYBuffer/GetAttr 'width buffer))
(define (XYBuffer/Height buffer)   (XYBuffer/GetAttr 'height buffer))
(define (XYBuffer/CurrentX buffer) (XYBuffer/GetAttr 'curx buffer))
(define (XYBuffer/CurrentY buffer) (XYBuffer/GetAttr 'cury buffer))
(define (XYBuffer/CurrentPoint buffer)
  (cons (XYBuffer/CurrentX buffer)
        (XYBuffer/CurrentY buffer)))

(define (XYBuffer/MVDrawChar x y char buffer)
  (let ((w (XYBuffer/Width buffer))
        (h (XYBuffer/Height buffer)))
    (cond ((> y h)
           ; if height-overflow we extend buffer at first, then redraw
           (XYBuffer/ExtendHeight (- y h) buffer)
           (XYBuffer/MVDrawChar x y char buffer))
          ((> x w)
           ; FIXME: width-overflow will be ignored
           'ignore)
          (else
            (XYBuffer/PutChar x y char buffer)))))
    
(define (XYBuffer/MVDrawString x y string buffer)
  (let loop ((string string) (x x) (y y))
    (if (string-null? s)
        'done
        (begin
          (XYBuffer/PutChar x y (string-head string 1) buffer)
          (loop       (+ x 1) y (string-tail string 1) buffer)))))

(define (object->string o)
  (cond
    ((symbol? o) (symbol->string o))
    ((number? o) (number->string o))))

(define (display-length o)
  (string-length (object->string o)))
    
(define (vector-println v)
  (vector-for-each (lambda (x) (display (q-guard x " "))) v)
  (newline))


;;; 
(define (XYBuffer/Flush buffer)
  )