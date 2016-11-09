;;;============================================================================
;;;=                                                                          =
;;;= miniMaxima - A toy implementation of Maxima-like Computer Algebra System =
;;;=                                                                          =
;;;============================================================================
(load-option 'format)

;;; file that already be loaded
(define MM/LoadedFile '())

(define (load-relative filename)
  (let ((full-path
         (format #f "~A~A" (directory-namestring (current-load-pathname)) filename)))
    (begin
      (if (not (member full-path MM/LoadedFile))
          (set! MM/LoadedFile (cons full-path MM/LoadedFile)))
      (load full-path))))

;;; require is same like load except that it wouln't load same file twice
(define (require filename)
  (let ((full-path
         (format #f "~A~A" (directory-namestring (working-directory-pathname)) filename)))
    (format #t "require file: ~A~%" full-path)
    (if (not (member full-path MM/LoadedFile))
        (begin
          (set! MM/LoadedFile (cons full-path MM/LoadedFile))
          (load full-path)))))


(define (require-relative filename)
  (let ((full-path
         (format #f "~A~A" (directory-namestring (current-load-pathname)) filename)))
    (format #t "require file: ~A~%" full-path)
    (if (not (member full-path MM/LoadedFile))
        (begin
          (set! MM/LoadedFile (cons full-path MM/LoadedFile))
          (load full-path)))))

(define Mackage/RepoDir
  (string-append MM/RootDir "/mackages/"))

(require-relative "src/lib/main.scm")
(require-relative "src/mackage/main.scm")

(use-mackage 'arith)