;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2023-03-05 20:24:45>

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
            "LET*-T"
            "DEFUN-T"

            "GET-LIBRARY"
            
            "BG"

            ))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
