;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-06-08 21:53:34>


(in-package macros)

(defvar *found-libraries* ())

(defun get-library (default-name)
  (let* ((command (format nil "ldconfig -p | grep ~a" default-name))
         (out (make-string-output-stream)))
    (handler-case
        (progn
          (uiop:run-program command :input nil :output out :ignore-error-status t)
          (let* ((output (get-output-stream-string out))
                 (entry (car (cl-utilities:split-sequence #\newline output)))
                 (sep (search "=> " entry)))
            (when (null sep)
              (error "Library ~a not found, please install it or check why ldconfig -p does not list it."
                     default-name))
            (let ((library-path (subseq entry (+ sep 3))))
              (log2:info "Found ~a ==> ~a" default-name library-path)
              (pushnew library-path *found-libraries* :test #'string-equal)
              library-path)))
      (uiop/run-program:subprocess-error (e)
        (error e)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
