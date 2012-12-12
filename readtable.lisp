;;;; readtable.lisp

(cl:in-package :cl-user)
(named-readtables:in-readtable :common-lisp)

(named-readtables:defreadtable :sharp-bq
  (:merge :standard)
  (:macro-char #\, #'sharp-bq.internal::xr-comma-macro)
  (:dispatch-macro-char #\# #\` #'sharp-bq.internal::xr-#\`-macro)
  (:case :upcase))
