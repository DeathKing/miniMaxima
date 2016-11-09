(define (use-mackage mkg)
  (let ((mkg-name
          (cond ((string? mkg) mkg)
                ((symbol? mkg) (symbol->string mkg))
                (else
                  (error "mkg should be a string or symbol -- " mkg)))))
    (let ((path (string-append Mackage/RepoDir mkg-name "/main.scm")))
      (if (file-exists? path)
          (load path)
          (error "file not exists for -- " path)))))
  