;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Last Modified  <michael 2022-01-30 12:19:21>

(defsystem "makros"
  :description "Macros"
  :in-order-to ((test-op (test-op "makros/test")))
  :default-component-class cl-source-file.cl
  :depends-on ("log2" "cl-utilities" "local-time")
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "base")
               (:file "json-parser")))

(defsystem "makros/test"
  :description "JSON parser and serializer tests"
  :depends-on ("makros")
  :default-component-class cl-source-file.cl
  :serial t
  :components ((:file "tests/json-parser-tests"))
  :perform (test-op (operation system)
             (declare (ignore operation system))
             (uiop:symbol-call :makros-json-tests :run-tests)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
