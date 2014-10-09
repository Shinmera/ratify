#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.toolkit)

(define-condition ratification-error (error)
  ((%message :initarg :message :initform NIL :accessor message)
   (%test-object :initarg :test-object :initform (error "Test-object required") :accessor test-object))
  (:report (lambda (c s) (format s "Error during ratification of ~s.~@[~%~a~]"
                                 (test-object c) (message c))))
  (:documentation "Error signalled if a test function hit an error in the format.
The TEST-OBJECT slot contains the object that failed to pass the test.
The MESSAGE slot contains a verbal explanation of what went wrong."))

(defun ratification-error (test-object &optional message &rest format-args)
  "Shorthand function to signal a RATIFICATION-ERROR."
  (error 'ratification-error
         :test-object test-object
         :message (when message (apply #'format NIL message format-args))))

(defun make-keyword (name)
  "Returns the keyword equivalent of the passed NAME."
  (let ((name (string name)))
    (or (find-symbol name "KEYWORD")
        (intern name "KEYWORD"))))

(defun true-alpha-p (char)
  "Returns T if the character is one of a-Z.

ALPHA-CHAR-P as per CLHS is not strictly limited to just a-Z and returns T
for undesired characters like ü on some implementations like SBCL."
  (or (char<= #\a char #\z)
      (char<= #\A char #\Z)))

(defun true-alphanumeric-p (char)
  "Returns T if the character is one of a-Z 0-9.

ALPHANUMERICP as per CLHS is not strictly limited to just a-Z 0-9 and returns T
for undesired characters like ü on some implementations like SBCL."
  (or (true-alpha-p char)
      (char<= #\0 char #\9)))
