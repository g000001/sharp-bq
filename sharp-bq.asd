;;;; sharp-bq.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)

(defsystem :sharp-bq
  :serial t
  :depends-on (:fiveam
               :named-readtables)
  :components ((:file "package")
               (:file "sharp-bq")
               (:file "readtable")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :sharp-bq))))
  (load-system :sharp-bq)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :sharp-bq.internal :sharp-bq))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

