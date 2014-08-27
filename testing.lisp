#|
 This file is a part of ratify
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.testing)

(define-condition test-failed (error)
  ((%test-name :initarg :test-name :initform (error "Test-name required") :accessor test-name)
   (%test-object :initarg :test-object :initform (error "Test-object required") :accessor test-object)
   (%cause :initarg :cause :initform NIL :accessor cause))
  (:report (lambda (c s) (format s "Testing ~s for ~a failed~:[.~;:~%~:*~a~]"
                                 (test-object c) (test-name c) (cause c)))))

(define-condition combined-error (error)
  ((errors :initarg :errors :initform () :accessor errors))
  (:report (lambda (c s) (format s "~:[No errors occurred.~;~:*Errors occurred: ~{~a~^~%~}~]"
                                 (errors c)))))

(defvar *tests* (make-hash-table))

(defun test (name)
  (gethash (make-keyword name) *tests*))

(defun (setf test) (function name)
  (setf (gethash (make-keyword name) *tests*) function))

(defmacro define-test (name (param) &body body)
  (let ((func-name (intern (format NIL "TEST-~a" name)))
        (pred-name (intern (format NIL "~a-P" name))))
    `(progn
       (setf (test ,(string name))
             (defun ,func-name (,param)
               (let ((,param ,param))
                 ,@body)
               ,param))
       ;; This is going to break on any lambda-vars, but I don't care right now.
       (defun ,pred-name (,param)
         (ignore-errors 
          (,func-name ,param))))))

(defmacro with-skipping (&body body)
  `(with-simple-restart (skip-error "Skip the error and continue.")
     ,@body))

(defun skippable-error (datum &rest arguments)
  (with-skipping
    (apply #'error datum arguments)))

(defmacro with-errors-combined (&body body)
  (let ((combined-error (gensym "COMBINED-ERROR")))
    `(let ((,combined-error (make-instance 'combined-error)))
       (prog1
           (handler-bind ((error #'(lambda (err)
                                     (push err (errors ,combined-error))
                                     (if (find-restart 'skip-error)
                                         (invoke-restart 'skip-error)
                                         (error ,combined-error)))))
             ,@body)
         (when (errors ,combined-error)
           (error ,combined-error))))))

(defun perform-test (test-name test-object)
  (with-skipping
    (handler-bind ((error #'(lambda (err)
                              (error 'test-failed :cause err :test-object test-object :test-name test-name))))
      (funcall (test test-name) test-object))))

(defmacro perform-tests (&body test-forms)
  `(progn
     ,@(loop with forms = ()
             for (test . objects) in test-forms
             do (dolist (object objects)
                  (push `(perform-test ,(make-keyword test) ,object) forms))
             finally (return (nreverse forms)))))

(defmacro perform-combined-tests (&body test-forms)
  `(with-errors-combined
     (perform-tests ,@test-forms)))
