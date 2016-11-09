;;;============================================================================
;;;=                                                                          =
;;;= miniMaxima - A toy implementation of Maxima-like Computer Algebra System =
;;;=                                                                          =
;;;============================================================================

(define (load-relative filename)
  (with-working-directory-pathname 
    (directory-namestring (current-load-pathname))
    (lambda () (load filename))))

;;; file that already be loaded
(define MM/LoadedFile '())

;;; require 
;;;
;;; require is same like load except that it wouln't load same file twice
(define (require filename)
  load)

(define (require-relative filename)
  load-relative)

(define Mackage/RepoDir
  (string-append MM/RootDir "/mackages/"))


(load-relative "src/lib/main.scm")
(load-relative "src/mackage/main.scm")

(use-mackage 'arith)