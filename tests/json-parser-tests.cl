(defpackage :makros-json-tests
  (:use :cl :macros)
  (:export :run-tests))

(in-package :makros-json-tests)

(defun serialize (value)
  (with-output-to-string (stream)
    (json stream value)))

(defun fails (function)
  (handler-case
      (progn (funcall function) nil)
    (error () t)))

(defun run-tests ()
  (let* ((special-string
           (coerce (list #\" #\\ (code-char 8) #\Tab #\Newline
                         (code-char 12) #\Return (code-char 1)
                         (code-char 233))
                   'string))
         (escaped-string
           (concatenate 'string
                        "\"" "\\\"" "\\\\" "\\b" "\\t" "\\n"
                        "\\f" "\\r" "\\u0001" (string (code-char 233)) "\""))
         (unicode-string
           (concatenate 'string
                        "A" (string #\Newline)
                        (string (code-char #x1f600)))))
    (assert (equal escaped-string (serialize special-string)))
    (assert (equal special-string (parse-json escaped-string)))
    (assert (equal unicode-string
                   (parse-json "\"A\\n\\uD83D\\uDE00\"")))
    (let ((high-surrogate (code-char #xd800))
          (low-surrogate (code-char #xdc00)))
      (when (and high-surrogate low-surrogate)
        (assert (equal (string high-surrogate) (parse-json "\"\\uD800\"")))
        (assert (equal (string low-surrogate) (parse-json "\"\\uDC00\"")))
        (assert (equal (concatenate 'string (string high-surrogate) "A")
                       (parse-json "\"\\uD800\\u0041\"")))
        (assert (equal "\"\\uD800\"" (serialize (string high-surrogate))))))
    (assert (equal "true" (serialize t)))
    (assert (equal "false" (serialize nil)))
    (assert (equal "true" (serialize 'true)))
    (assert (equal "false" (serialize 'false)))
    (assert (equal "null" (serialize 'json-null)))
    (assert (equal "1.2500" (serialize 1.25d0)))
    (assert (equal "0.5" (serialize 1/2)))
    (assert (fails (lambda () (serialize #c(1 2)))))
    (assert (eq 'true (parse-json "true")))
    (assert (eq 'false (parse-json "false")))
    (assert (eq 'json-null (parse-json "null")))
    (assert (equal "[0,32,36]" (serialize '(0 32 36))))
    (assert (equal "[0.0000,0.0000,1.5000]"
                   (serialize '(0.0 0.0 1.5))))
    (assert (equal "[]" (serialize #())))
    (assert (equal "[[1,2],[3,4]]"
                   (serialize (make-array '(2 2) :initial-contents '((1 2) (3 4))))))
    (let ((object (parse-json "{\"a\\\"b\":1,\"other\":[true,false,null]}")))
      (assert (= 1 (joref object "a\"b")))
      (setf (joref object "a\"b") 2)
      (assert (= 2 (joref object "a\"b")))
      (assert (equal "{\"a\\\"b\": 2,\"other\": [true,false,null]}"
                     (serialize object))))
    (dolist (number '("0" "-0" "1" "-12" "0.1" "-0.5" "1e2" "2E-3"))
      (assert (numberp (parse-json number))))
    (dolist (invalid '("" " " "+" "-" ".1" "01" "-01" "1." "1e" "1e+"
                       "NaN" "Infinity" "truex" "null0" "1," "[1,]" "{\"a\":1,}" "{\"a\" 1}"
                       "true false" "\"\\x\"" "\"\\u12\""))
      (unless (fails (lambda () (parse-json invalid)))
        (error "Invalid JSON input was accepted: ~s" invalid)))
    (assert (fails (lambda ()
                     (parse-json (concatenate 'string "\"" (string (code-char 1)) "\"")))))
    (assert (fails (lambda () (parse-json "1 trailing"))))
    (multiple-value-bind (first end) (parse-json "1 {\"a\":2}" 0)
      (assert (= 1 first))
      (multiple-value-bind (second final) (parse-json "1 {\"a\":2}" end)
        (assert (= 2 (joref second "a")))
        (assert (= final (length "1 {\"a\":2}")))))
    (let ((cycle (make-array 1)))
      (setf (aref cycle 0) cycle)
      (assert (fails (lambda () (serialize cycle)))))
    t))
