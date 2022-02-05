;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Last Modified  <michael 2022-01-30 12:19:21>

(defsystem "makros"
  :description "Macros"
  :default-component-class cl-source-file.cl
  :depends-on ("log2" "cl-utilities" "local-time")
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "base")
               (:file "json-parser")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

