#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.parsing)

(defvar *parsers* (make-hash-table)
  "Hash map mapping keywords to parsing functions.
A parse function should take one argument.")

(defun parser (name)
  "Returns the function associated with the NAME.
If no such parser can be found, #'IDENTITY is returned.
The name is converted to a keyword.

SETF-able."
  (or (gethash (make-keyword name) *parsers*)
      #'identity))

(defun (setf parser) (function name)
  "Sets a function to be used for a certain parser.
The name is converted to a keyword."
  (setf (gethash (make-keyword name) *parsers*) function))

(defmacro define-parser (name (param start end) &body body)
  "Defines a new parse function with NAME.
PARAM will be bound to the object to parse, which is a string unless otherwise
specified, START to the starting index (inc) and END to the ending index (exc).

This function creates two other functions automatically:
PARSE-name This is the main test function. If the test fails, an error of
          type RATIFICATION-ERROR should be returned. If the test succeeds
          the argument passed to it is always returned."
  (let ((func-name (intern (format NIL "PARSE-~a" name))))
    `(setf
      (parser ,(string name))
      (defun ,func-name (,param &optional (,start 0) (,end (length ,param)))
        (declare (ignorable ,start ,end))
        ,@body))))

(defun parse (parser-name object)
  "Attempts to parse OBJECT using the parser named by PARSER-NAME.

Automatically establishes a SKIP-ERROR restart as per WITH-SKIPPING.
Performs exactly two operations:
1) Call the test function of name PARSER-NAME on the object
2) Call the parse function of name PARSER-NAME on the object"
  (with-skipping
    (ratify-testing::perform-test-no-skip parser-name object)
    (funcall (parser parser-name) object)))

(defmacro with-parsed-forms (parse-forms &body body)
  "Performs a series of parsing operations on objects and rebinds their symbols to the results.

PARSE-FORMS ::= PARSE-FORM*
PARSE-FORM  ::= (parser-name object*)
See PARSE.

The parse operations are performed within WITH-ERRORS-COMBINED.
As such all parse operations are always performed and only one or no
conditions are signalled as part of the parsing.
See WITH-ERRORS-COMBINED."
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
