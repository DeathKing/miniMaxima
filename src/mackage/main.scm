
(define Mackage/InstalledMkg '())

(define Mackage/MakeVersion list)

(define (Mackage/Define name sym version desc)
  (let ((record (list name sym version desc)))
    (set! Mackage/InstalledMkg
          (cons (cons sym record) Mackage/InstalledMkg))))

(define (Mackage/Presented? sym)
  (assoc sym Mackage/InstalledMkg))

(define (use-mackage mkg)
  (if (not (Mackage/Presented? mkg))
      (let ((mkg-name
          (cond ((string? mkg) mkg)
                ((symbol? mkg) (symbol->string mkg))
                (else
                  (error "mkg should be a string or symbol -- " mkg)))))
        (let ((path (string-append Mackage/RepoDir mkg-name "/main.scm")))
          (if (file-exists? path)
              (require-relative path)
              (error "file not exists for -- " path))))))

(define Mackage/Use use-mackage)