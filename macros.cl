;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2021
;;; Last Modified <michael 2021-06-08 21:19:29>

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

(defun parse-lambda-list (l)
  (flet ((make-argtypespec (other-args)
           (do*
            ((mode :required)
             (result (list))
             (unprocessed other-args (cdr unprocessed))
             (next (car unprocessed) (car unprocessed)))
            ((null unprocessed)
             (reverse result))
             (cond
               ((eq next '&optional)
                (push next result)
                (setf mode :optional))
               ((eq next '&key)
                (push next result)
                (setf mode :key))
               ((member next '(&rest &aux))
                (error "Unsupported lambda list ~a" unprocessed))
               (t
                (case mode
                  ((:required :optional)
                   (push t result))
                  (:key
                   (cond
                     ((atom next)
                      (push (list (intern (symbol-name next) :keyword)
                                  t)
                            result))
                     (t
                      (push (list (intern (symbol-name (car next)) :keyword)
                                  ;; Keyword type might be derived from inital value, use T for now
                                  t)
                            result))))))))))
    (loop
      :for (a . r) :on l
      :while (consp a)
      :collect (list (cadr a) (car a)) :into declarations
      :collect (cadr a) :into fargtypes
      :collect (car a) :into args
      :finally (let ((other-args
                       (if (atom a) (cons a r) r)))
                 (return (values (append args other-args)
                                 (append fargtypes (make-argtypespec other-args))
                                 declarations))))))
  
(defmacro defun-t (name return-type (&rest typed-arglist) &body body)
  (multiple-value-bind (lambda-list argtypes declarations)
      (parse-lambda-list typed-arglist)
    `(progn
       (declaim (ftype (function ,argtypes ,return-type) ,name))
       (defun ,name ,lambda-list (declare ,@declarations) ,@body))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
