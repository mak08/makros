;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2025-10-16 23:04:25>

(defpackage macros
  (:use :cl :sb-mop :local-time)
  (:export  "CHECK-EQUAL"
            "CHECK-EPS-EQUAL"

            "JSON"
            "JSON%"

            "PARSE-JSON"
            "PARSE-JSON-FILE"

            "JOREF"
            "JSON-FIELD-NAME"
            "JSON-OBJECT-FIELDS"
            "FALSE"
            "TRUE"

            "MAKE-JSON-OBJECT"
            "MAKE-JSON-FIELD"
            
            "LET-T"
            "LET*-T"
            "DEFUN-T"

            "GET-LIBRARY"
            
            "BG"

            ))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
