#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.testing)

(define-condition test-failed (error)
  ((%test-name :initarg :test-name :initform (error "Test-name required") :accessor test-name)
   (%test-object :initarg :test-object :initform (error "Test-object required") :accessor test-object)
   (%cause :initarg :cause :initform NIL :accessor cause))
  (:report (lambda (c s) (format s "Testing ~s for ~a failed~:[.~;:~%~:*~a~]"
                                 (test-object c) (test-name c) (cause c))))
  (:documentation "Condition signalled when a test fails.
The TEST-NAME slot specifies the name of the test that was run.
The TEST-OBJECT slot contains the object that failed the test.
The CAUSE slot contains the original error object, usually of type RATIFICATION-ERROR."))

(define-condition combined-error (error)
  ((errors :initarg :errors :initform () :accessor errors))
  (:report (lambda (c s) (format s "~:[No errors occurred.~;~:*Errors occurred: ~{~%> ~a~}~]"
                                 (errors c))))
  (:documentation "An error object that holds a combination of other errors.
Used to test multiple things before unwinding the stack."))

(defvar *tests* (make-hash-table)
  "Hash map mapping keywords to testing functions.
A test function should take one argument.")

(defun test (name)
  "Returns the function associated with the NAME.
If no such test can be found, an error is signalled.
The name is converted to a keyword.

SETF-able."
  (or (gethash (make-keyword name) *tests*)
      (error "No such test ~s." name)))

(defun (setf test) (function name)
  "Sets a function to be used for a certain test.
The name is converted to a keyword."
  (setf (gethash (make-keyword name) *tests*) function))

(defmacro define-test (name (param start end) &body body)
  "Defines a new test function with NAME.
PARAM will be bound to the object to test, which is a string unless otherwise
specified, START to the starting index (inc) and END to the ending index (exc).

This function creates two other functions automatically:
TEST-name This is the main test function. If the test fails, an error of
          type RATIFICATION-ERROR should be returned. If the test succeeds
          the argument passed to it is always returned.
name-P    Equivalent to the TEST- function, except that it simply returns
          NIL on failure instead of signalling an error."
  (let ((func-name (intern (format NIL "TEST-~a" name)))
        (pred-name (intern (format NIL "~a-P" name))))
    `(progn
       (setf (test ,(string name))
             (defun ,func-name (,param &optional (,start 0) (,end (length ,param)))
               (declare (ignorable ,start ,end))
               ,@(when (stringp (car body))
                   (list (pop body)))
               (let ((,param ,param))
                 ,@body)
               ,param))
       (defun ,pred-name (,param &optional (,start 0) (,end (length ,param)))
         ,(format NIL "Predicate version of ~a, returns the passed value on success, NIL on error." func-name)
         (ignore-errors 
          (,func-name ,param ,start ,end))))))

(defmacro with-skipping (&body body)
  "Marks the body as being skippable if an error occurs within.
This establishes the restart SKIP-ERROR."
  `(with-simple-restart (skip-error "Skip the error and continue.")
     ,@body))

(defun skippable-error (datum &rest arguments)
  "Signals a skippable error as per WITH-SKIPPING."
  (with-skipping
    (apply #'error datum arguments)))

(defmacro with-errors-combined (&body body)
  "Executes the body with special error handling.
Errors are gathered in a COMBINED-ERROR, which is finally signalled once
the body finishes or an error occurs and no SKIP-ERROR restart can be found.

If no errors occur within the body, the last value of the body is returned
as per PROGN."
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

(defun perform-test-no-skip (test-name test-object)
  (handler-bind ((error #'(lambda (err)
                            (error 'test-failed :cause err :test-object test-object :test-name test-name))))
    (funcall (test test-name) test-object)))

(defun perform-test (test-name test-object)
  "Performs the test named by TEST-NAME on TEST-OBJECT.

Automatically establishes a SKIP-ERROR restart and resignals any error
as a new error of type TEST-FAILED."
  (with-skipping
    (perform-test-no-skip test-name test-object)))

(defmacro perform-tests (&body test-forms)
  "Performs a series of tests.

TEST-FORMS ::= TEST-FORM*
TEST-FORM  ::= (test-name test-object*)
See TEST."
  `(progn
     ,@(loop with forms = ()
             for (test . objects) in test-forms
             do (dolist (object objects)
                  (push `(perform-test ,(make-keyword test) ,object) forms))
             finally (return (nreverse forms)))
     T))

(defmacro perform-combined-tests (&body test-forms)
  "Same as PERFORM-TESTS, except with WITH-ERRORS-COMBINED in effect."
  `(with-errors-combined
     (perform-tests ,@test-forms)))
