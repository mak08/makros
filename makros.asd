;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Last Modified  <michael 2021-03-22 23:25:46>

(defsystem "makros"
  :description "Macros"
  :default-component-class cl-source-file.cl
  :depends-on ("log2")
  :serial t
  :components ((:file "macros")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

