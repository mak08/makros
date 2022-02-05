;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2022-01-30 13:49:14>

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
            
            "LET-T"
            "DEFUN-T"

            "GET-LIBRARY"
            
            "BG"

            ))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
