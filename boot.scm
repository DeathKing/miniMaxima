(define MM/RootDir '())

(argument-command-line-parser "-root"
                              #t
                              (lambda (root-path)
                                (set! MM/RootDir root-path)))