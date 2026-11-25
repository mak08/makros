;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2022
;;; Last Modified <michael 2026-11-23 23:38:00>

(in-package :macros)

(defstruct json-object fields)
(defstruct json-field name value)

(defvar *float-digits* 4)

(defun json (stream thing &key (preserve-slotname-case nil))
  "Serialize a Lisp value to JSON; TRUE, FALSE, and JSON-NULL represent literals."
  (let ((seen-ht (make-hash-table :test #'eq))
        (slotname-formatter (if preserve-slotname-case #'identity #'string-downcase)))
    (declare (special seen-ht slotname-formatter))
    (json% stream thing)))

(defgeneric json% (stream thing))

(defun write-json-string (stream string)
  (unless (stringp string)
    (error "A JSON string must be a Lisp string, not ~s" string))
  (write-char #\" stream)
  (loop :for character :across string
        :for code = (char-code character)
        :do (cond
              ((eql character #\") (write-string "\\\"" stream))
              ((eql character #\\) (write-string "\\\\" stream))
              ((eql code 8) (write-string "\\b" stream))
              ((eql code 9) (write-string "\\t" stream))
              ((eql code 10) (write-string "\\n" stream))
              ((eql code 12) (write-string "\\f" stream))
              ((eql code 13) (write-string "\\r" stream))
              ((< code 32) (format stream "\\u~4,'0X" code))
              ((<= #xd800 code #xdfff)
               (format stream "\\u~4,'0X" code))
              (t (write-char character stream))))
  (write-char #\" stream))

(defun with-json-container (thing function)
  (declare (special seen-ht))
  (when (gethash thing seen-ht)
    (error "Circular objects are not supported"))
  (setf (gethash thing seen-ht) t)
  (unwind-protect (funcall function)
    (remhash thing seen-ht)))

(defmethod json% (stream (thing t))
  (write-json-string stream (princ-to-string thing)))

(defmethod json% (stream (thing integer))
  (format stream "~d" thing))

(defmethod json% (stream (thing ratio))
  (write-string (format nil "~f" thing) stream))

(defmethod json% (stream (thing number))
  (error "Complex numbers cannot be represented as JSON: ~s" thing))

(defun write-json-float (stream thing)
  (handler-case
      (integer-decode-float thing)
    (error ()
      (error "Non-finite floating point values cannot be represented as JSON: ~s" thing)))
  (unless (and (integerp *float-digits*) (not (minusp *float-digits*)))
    (error "*FLOAT-DIGITS* must be a non-negative integer, not ~s" *float-digits*))
  (format stream "~v$" *float-digits* thing))

(defmethod json% (stream (thing float))
  (write-json-float stream thing))

(defmethod json% (stream (thing timestamp))
  (write-json-string stream (format-datetime nil thing)))

(defun format-datetime (stream timestamp &key (timezone +utc-zone+))
  (format stream "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
          (timestamp-year timestamp :timezone timezone)
          (timestamp-month timestamp :timezone timezone)
          (timestamp-day timestamp :timezone timezone)
          (timestamp-hour timestamp :timezone timezone)
          (timestamp-minute timestamp :timezone timezone)
          (timestamp-second timestamp :timezone timezone)))

(defmethod json% (stream (thing string))
  (write-json-string stream thing))

(defmethod json% (stream (thing (eql nil)))
  (write-string "false" stream))

(defmethod json% (stream (thing (eql t)))
  (write-string "true" stream))

(defmethod json% (stream (thing (eql 'true)))
  (declare (ignore thing))
  (write-string "true" stream))

(defmethod json% (stream (thing (eql 'false)))
  (declare (ignore thing))
  (write-string "false" stream))

(defmethod json% (stream (thing (eql 'json-null)))
  (declare (ignore thing))
  (write-string "null" stream))

(defmethod json% (stream (thing structure-object))
  (with-json-container
   thing
   (lambda ()
     (declare (special slotname-formatter))
     (let ((slots (filtered-slots thing)))
       (write-char #\{ stream)
       (loop :for (slot-name . rest) :on slots
             :do (unless (slot-boundp thing slot-name)
                   (error "Unbound slot ~s in ~s cannot be serialized"
                          slot-name thing))
                 (unless (eq slot-name (car slots))
                   (write-char #\, stream))
                 (write-char #\Newline stream)
                 (write-json-string stream
                                    (funcall slotname-formatter
                                             (symbol-name slot-name)))
                 (write-char #\: stream)
                 (write-char #\Space stream)
                 (json% stream (slot-value thing slot-name))
             :finally (when slots (write-char #\Newline stream)))
       (write-char #\} stream)))))

(defun filtered-slots (thing)
  (loop :for slot :in (class-slots (class-of thing))
        :for name = (slot-definition-name slot)
        :for symbol-name = (symbol-name name)
        :unless (and (plusp (length symbol-name))
                     (char= (char symbol-name (1- (length symbol-name))) #\%))
          :collect name))

(defmethod json% (stream (thing standard-object))
  (with-json-container
   thing
   (lambda ()
     (let ((slots (remove-if
                 (lambda (slot)
                   (member (symbol-name (slot-definition-name slot))
                           '("lock--mode") :test #'string-equal))
                 (class-slots (class-of thing)))))
       (write-char #\{ stream)
       (loop :for slot :in slots
             :for slot-name = (slot-definition-name slot)
             :for first = t :then nil
             :do (unless first (write-char #\, stream))
                 (write-json-string stream (string-downcase (symbol-name slot-name)))
                 (write-char #\: stream)
                 (write-char #\Space stream)
                 (json% stream (if (slot-boundp thing slot-name)
                                   (slot-value thing slot-name)
                                   'default)))
       (write-char #\} stream)))))

(defmethod json% (stream (thing json-object))
  (with-json-container
   thing
   (lambda ()
     (write-char #\{ stream)
     (loop :for field :in (json-object-fields thing)
           :for first = t :then nil
           :do (unless first (write-char #\, stream))
               (write-json-string stream (json-field-name field))
               (write-char #\: stream)
               (write-char #\Space stream)
               (json% stream (json-field-value field)))
     (write-char #\} stream))))

(defmethod json% (stream (thing list))
  (when (and thing (null (list-length thing)))
    (error "Circular lists are not supported"))
  (with-json-container
   thing
   (lambda ()
     (write-char #\[ stream)
     (loop :for element :in thing
           :for first = t :then nil
           :do (unless first (write-char #\, stream))
               (json% stream element))
     (write-char #\] stream))))

(defmethod json% (stream (thing vector))
  (with-json-container
   thing
   (lambda ()
     (write-char #\[ stream)
     (loop :for index :below (length thing)
           :do (when (plusp index) (write-char #\, stream))
               (json% stream (aref thing index)))
     (write-char #\] stream))))

(defun write-json-array-dimension (stream array dimensions dimension index)
  (write-char #\[ stream)
  (loop :for subscript :below (nth dimension dimensions)
        :do (when (plusp subscript) (write-char #\, stream))
            (if (= dimension (1- (length dimensions)))
                (json% stream (row-major-aref array (+ index subscript)))
                (write-json-array-dimension
                 stream array dimensions (1+ dimension)
                 (+ index (* subscript
                             (reduce #'* (subseq dimensions (1+ dimension))
                                     :initial-value 1))))))
  (write-char #\] stream))

(defmethod json% (stream (thing array))
  (with-json-container
   thing
   (lambda ()
     (let ((dimensions (array-dimensions thing)))
       (if dimensions
           (write-json-array-dimension stream thing dimensions 0 0)
           (progn
             (write-char #\[ stream)
             (json% stream (row-major-aref thing 0))
             (write-char #\] stream)))))))

(defmethod json% (stream (thing json-field))
  (declare (ignore stream))
  (error "A JSON field must be contained in a JSON object, not serialized directly: ~s"
         thing))

(defmethod print-object ((thing json-object) stream)
  (format stream "<~{~a~^, ~}>" (json-object-fields thing)))

(defmethod print-object ((thing json-field) stream)
  (format stream "~a: ~a"
          (json-field-name thing)
          (json-field-value thing)))

(defun set-json-object-field (json-object field-name value)
  (unless (stringp field-name)
    (error "A JSON object field name must be a string, not ~s" field-name))
  (setf (json-object-fields json-object)
        (cons (make-json-field :name field-name :value value)
              (remove field-name (json-object-fields json-object)
                      :key #'json-field-name
                      :test #'string=))))

(define-setf-expander joref (json-object field-name)
  (let ((object-temp (gensym "OBJECT-"))
        (field-temp (gensym "FIELD-"))
        (store-temp (gensym "VALUE-")))
    (values (list object-temp field-temp)
            (list json-object field-name)
            (list store-temp)
            `(set-json-object-field ,object-temp ,field-temp ,store-temp)
            `(joref ,object-temp ,field-temp))))

(defun joref (json-object field-name)
  (loop :for field :in (json-object-fields json-object)
        :when (string= (json-field-name field) field-name)
          :return (json-field-value field)))

(defun parse-json-file (filename)
  (log2:trace "Loading JSON from ~a~%" filename)
  (with-open-file (stream filename :element-type 'character :external-format :utf-8)
    (parse-json (with-output-to-string (string)
                  (loop :for character = (read-char stream nil nil)
                        :while character
                        :do (write-char character string))))))

(defun parse-json (string &optional (start 0 start-supplied-p))
  "Parse one JSON value and return its ending position.
Arrays are vectors, objects are JSON-OBJECT values, and literals are TRUE,
FALSE, and JSON-NULL. Supplying START allows parsing consecutive values."
  (unless (stringp string)
    (error "JSON input must be a string, not ~s" string))
  (unless (and (integerp start) (<= 0 start (length string)))
    (error "Invalid JSON start position ~s" start))
  (let ((pos start))
    (declare (special pos))
    (whitespace string)
    (when (>= pos (length string))
      (json-parse-error string "Expected a JSON value"))
    (let ((value (parse-json-expr string)))
      (whitespace string)
      (when (and (not start-supplied-p) (< pos (length string)))
        (json-parse-error string "Unexpected trailing data"))
      (values value pos))))

(defun json-parse-error (string message)
  (declare (ignore string) (special pos))
  (error "~a at position ~d in JSON input" message pos))

(defun peek (string character)
  (declare (special pos))
  (and (< pos (length string))
       (char= (aref string pos) character)))

(defun expect (string character)
  (declare (special pos))
  (unless (peek string character)
    (json-parse-error string (format nil "Expected ~s" character)))
  (incf pos))

(defun is-white (character)
  (member character '(#\Space #\Tab #\Return #\Newline) :test #'char=))

(defun whitespace (string)
  (declare (special pos))
  (loop :while (and (< pos (length string))
                    (is-white (aref string pos)))
        :do (incf pos)))

(defun json-digit-p (character)
  (and (char<= #\0 character) (char<= character #\9)))

(defun json-nonzero-digit-p (character)
  (and (char<= #\1 character) (char<= character #\9)))

(defun parse-json-expr (string)
  (declare (special pos))
  (when (>= pos (length string))
    (json-parse-error string "Unexpected end of input"))
  (case (aref string pos)
    (#\{ (parse-json-object string))
    (#\[ (parse-json-array string))
    (#\" (parse-json-string string))
    (#\t (parse-json-literal string "true" 'true))
    (#\f (parse-json-literal string "false" 'false))
    (#\n (parse-json-literal string "null" 'json-null))
    (t (if (or (char= (aref string pos) #\-)
               (json-digit-p (aref string pos)))
           (parse-json-number string)
           (json-parse-error string "Invalid JSON value")))))

(defun parse-json-literal (string literal value)
  (declare (special pos))
  (loop :for character :across literal
        :do (expect string character))
  value)

(defun parse-json-array (string)
  (declare (special pos))
  (expect string #\[)
  (whitespace string)
  (let ((elements '()))
    (unless (peek string #\])
      (loop
        (push (parse-json-expr string) elements)
        (whitespace string)
        (if (peek string #\,)
            (progn
              (incf pos)
              (whitespace string)
              (when (peek string #\])
                (json-parse-error string "Trailing comma in array")))
            (return))))
    (expect string #\])
    (make-array (length elements) :initial-contents (nreverse elements))))

(defun parse-json-object (string)
  (declare (special pos))
  (expect string #\{)
  (whitespace string)
  (let ((fields '()))
    (unless (peek string #\})
      (loop
        (unless (peek string #\")
          (json-parse-error string "Expected a quoted object field name"))
        (push (parse-json-field string) fields)
        (whitespace string)
        (if (peek string #\,)
            (progn
              (incf pos)
              (whitespace string)
              (when (peek string #\})
                (json-parse-error string "Trailing comma in object")))
            (return))))
    (expect string #\})
    (make-json-object :fields (nreverse fields))))

(defun parse-json-field (string)
  (declare (special pos))
  (let ((name (parse-json-string string)))
    (whitespace string)
    (expect string #\:)
    (whitespace string)
    (make-json-field :name name :value (parse-json-expr string))))

(defun parse-json-string (string)
  (declare (special pos))
  (expect string #\")
  (with-output-to-string (result)
    (loop
      (when (>= pos (length string))
        (json-parse-error string "Unterminated JSON string"))
      (let ((character (aref string pos)))
        (incf pos)
        (cond
          ((char= character #\")
           (return))
          ((< (char-code character) 32)
           (json-parse-error string "Unescaped control character in JSON string"))
          ((<= #xd800 (char-code character) #xdfff)
           (json-parse-error string "Unpaired surrogate character in JSON string"))
          ((char= character #\\)
           (when (>= pos (length string))
             (json-parse-error string "Incomplete escape sequence"))
           (let ((escaped (aref string pos)))
             (incf pos)
             (case escaped
               (#\" (write-char #\" result))
               (#\\ (write-char #\\ result))
               (#\/ (write-char #\/ result))
               (#\b (write-char (code-char 8) result))
               (#\f (write-char (code-char 12) result))
               (#\n (write-char #\Newline result))
               (#\r (write-char #\Return result))
               (#\t (write-char #\Tab result))
               (#\u (write-char (parse-json-unicode-escape string) result))
               (t (json-parse-error string "Invalid string escape")))))
          (t (write-char character result)))))))

(defun parse-json-unicode-escape (string)
  (declare (special pos))
  (let ((code (parse-json-hex-quad string)))
    (cond
      ((<= #xd800 code #xdbff)
       (let ((after-high pos))
         (if (and (< (1+ pos) (length string))
                  (char= (aref string pos) #\\)
                  (char= (aref string (1+ pos)) #\u))
             (progn
               (incf pos 2)
               (let ((low (parse-json-hex-quad string)))
                 (if (<= #xdc00 low #xdfff)
                     (or (code-char (+ #x10000
                                       (ash (- code #xd800) 10)
                                       (- low #xdc00)))
                         (json-parse-error string "Unicode code point is not supported"))
                     (progn
                       (setf pos after-high)
                       (json-unicode-character code)))))
             (json-unicode-character code))))
      ((<= #xdc00 code #xdfff)
       (json-unicode-character code))
      (t (json-unicode-character code)))))

(defun json-unicode-character (code)
  (or (code-char code)
      (error "Unicode code point U+~4,'0X is not supported" code)))

(defun parse-json-hex-quad (string)
  (declare (special pos))
  (let ((code 0))
    (dotimes (index 4 code)
      (when (>= pos (length string))
        (json-parse-error string "Incomplete Unicode escape"))
      (let* ((character (aref string pos))
             (digit (cond
                      ((char<= #\0 character #\9)
                       (- (char-code character) (char-code #\0)))
                      ((char<= #\a character #\f)
                       (+ 10 (- (char-code character) (char-code #\a))))
                      ((char<= #\A character #\F)
                       (+ 10 (- (char-code character) (char-code #\A)))))))
        (unless digit
          (json-parse-error string "Invalid hexadecimal digit in Unicode escape"))
        (setf code (+ (* code 16) digit))
        (incf pos)))))

(defun parse-json-number (string)
  (declare (special pos))
  (let ((start pos))
    (when (peek string #\-)
      (incf pos))
    (cond
      ((peek string #\0)
       (incf pos)
       (when (and (< pos (length string))
                  (json-digit-p (aref string pos)))
         (json-parse-error string "Leading zero in JSON number")))
      ((and (< pos (length string))
            (json-nonzero-digit-p (aref string pos)))
       (loop :while (and (< pos (length string))
                         (json-digit-p (aref string pos)))
             :do (incf pos)))
      (t (json-parse-error string "Expected digit in JSON number")))
    (when (peek string #\.)
      (incf pos)
      (unless (and (< pos (length string))
                   (json-digit-p (aref string pos)))
        (json-parse-error string "Expected digit after decimal point"))
      (loop :while (and (< pos (length string))
                        (json-digit-p (aref string pos)))
            :do (incf pos)))
    (when (or (peek string #\e) (peek string #\E))
      (incf pos)
      (when (or (peek string #\+) (peek string #\-))
        (incf pos))
      (unless (and (< pos (length string))
                   (json-digit-p (aref string pos)))
        (json-parse-error string "Expected exponent digit"))
      (loop :while (and (< pos (length string))
                        (json-digit-p (aref string pos)))
            :do (incf pos)))
    (when (and (< pos (length string))
               (not (or (is-white (aref string pos))
                        (find (aref string pos) ",]}" :test #'char=))))
      (json-parse-error string "Invalid character after JSON number"))
    (let ((token (subseq string start pos)))
      (handler-case
          (let ((*read-default-float-format* 'double-float)
                (*read-eval* nil)
                (*readtable* (copy-readtable nil)))
            (multiple-value-bind (number end) (read-from-string token nil nil)
              (unless (and (= end (length token)) (numberp number))
                (json-parse-error string "Invalid JSON number"))
              number))
        (error (condition)
          (json-parse-error string
                            (format nil "Invalid JSON number ~s (~a)" token condition)))))))
