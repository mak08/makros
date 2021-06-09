;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Last Modified  <michael 2021-06-08 21:18:35>

(defsystem "makros"
  :description "Macros"
  :default-component-class cl-source-file.cl
  :depends-on ("log2" "cl-utilities")
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "base")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

