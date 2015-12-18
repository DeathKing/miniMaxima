;;; MACAGE is a simple package management for miniMaxima

;;; macage install macage-name version=""
;;; macage remove macage-name
;;; macage upgrade

(define macage/installed-path
  (list user-homepath
        default-package-path)

(define macage/install
  (lambda (package-name)
    ))