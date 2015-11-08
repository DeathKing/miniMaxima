(provide-package '3dplot)

(pkg/defun plot
  (funexp   'string
   variable 'string
   from     'real
   to       'real
   step     'real)
  (let [(lzf (lzfun/build funexp variable))]
    (if (real/gt from to))))