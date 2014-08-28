#|
 This file is a part of ratify
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.parsing)

(defvar *parsers* (make-hash-table))

(defun parser (name)
  (or (gethash (make-keyword name) *parsers*)
      #'identity))

(defun (setf parser) (function name)
  (setf (gethash (make-keyword name) *parsers*) function))

(defmacro define-parser (name (param) &body body)
  (let ((func-name (intern (format NIL "PARSE-~a" name))))
    `(progn
       (setf (parser ,(string name))
             (defun ,func-name (,param)
               ,@body)))))

(defun parse (parser-name object)
  (with-skipping
    (funcall (test parser-name) object)
    (funcall (parser parser-name) object)))

(defmacro with-parsed-forms (parse-forms &body body)
  `(destructuring-bind ,(loop with vars = ()
                              for (test . objects) in parse-forms
                              do (loop for object in objects
                                       do (push object vars))
                              finally (return (nreverse vars)))
       (with-errors-combined
         (list
          ,@(loop with forms = ()
                  for (test . objects) in parse-forms
                  do (dolist (object objects)
                       (push `(parse ,(make-keyword test) ,object) forms))
                  finally (return (nreverse forms)))))
     ,@body))
