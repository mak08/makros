;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-03-22 23:25:57>

(defpackage macros
  (:use :cl)
  (:export "LET-T"
           "DEFUN-T"))

(in-package macros)

(defmacro let-t ((&rest typed-bindings) &body body)
  (loop
    :for (var type value) :in typed-bindings
    :collect (list var value) :into bindings
    :collect (list type var) :into declarations
    :finally (return
               `(let ,bindings
                  (declare ,@declarations)
                  ,@body))))

(defmacro defun-t (name return-type (&rest typed-args) &body body)
  (loop
    :for (var type) :in typed-args
    :collect (list type var) :into declarations
    :collect var :into args
    :finally (return
               `(progn
                  (declaim (ftype (function ,(mapcar #'car declarations) ,return-type) ,name))
                  (defun ,name ,args (declare ,@declarations) ,@body)))))


(defun-t x fixnum ((x fixnum))
  (let ((x (* x x)))
    x))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
