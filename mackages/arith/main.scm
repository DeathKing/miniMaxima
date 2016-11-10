(Mackage/Define
  "Arith"
  'arith
  (Mackage/MakeVersion 0 0 1)
  "Simple arithmetic support for both syntax and sematics.")

;;; Presentation && Syntax category manipulation 
(require-relative "basic.scm")
(require-relative "poly.scm")
(require-relative "derivative.scm")

;;; Semantics evaluation
(require-relative "env.scm")
(require-relative "eval.scm")

;;; Output/ Tex Format
(require-relative "box.scm")