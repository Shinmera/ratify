#|
 This file is a part of ratify
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.toolkit)

(define-condition ratification-error (error)
  ((%message :initarg :message :initform NIL :accessor message)
   (%test-object :initarg :test-object :initform (error "Test-object required") :accessor test-object))
  (:report (lambda (c s) (format s "Error during ratification of ~s.~@[~%~a~]"
                                 (test-object c) (message c)))))

(defun ratification-error (test-object &optional message &rest format-args)
  (error 'ratification-error
         :test-object test-object
         :message (when message (apply #'format NIL message format-args))))

(defun make-keyword (name)
  (let ((name (string name)))
    (or (find-symbol name "KEYWORD")
        (intern name "KEYWORD"))))

(defun true-alpha-p (char)
  (or (char<= #\a char #\z)
      (char<= #\A char #\Z)))

(defun true-alphanumeric-p (char)
  (or (true-alpha-p char)
      (char<= #\0 char #\9)))
