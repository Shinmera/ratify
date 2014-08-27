#|
 This file is a part of ratify
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.types)

(define-test bit (bit)
  (unless (or (string= bit "1")
              (string= bit "0"))
    (ratification-error bit "A bit must be either 0 or 1.")))

(define-test unsigned-integer (integer)
  (when (= 0 (length integer))
    (ratification-error integer "An integer must be at least one digit."))
  (test-numeric integer))

(define-test integer (integer)
  (or (cl-ppcre:register-groups-bind (integer) ("^[+-]?(.*)$" integer)
        (test-unsigned-integer integer))
      (ratification-error integer "An integer must be a number optionally preceded by + or -.")))

(define-test ratio (ratio)
  (or (cl-ppcre:register-groups-bind (numerator NIL denominator) ("^[+-]?(.*)/(.*)$" ratio)
        (test-unsigned-integer numerator)
        (test-unsigned-integer denominator))
      (ratification-error ratio "A ratio must optionally start with + or -, followed by two integers separated by a forward slash.")))

(define-test rational (rational)
  (or (cl-ppcre:register-groups-bind (numerator NIL denominator) ("^[+-]?(.*)(/(.*))?$" rational)
        (test-unsigned-integer numerator)
        (test-unsigned-integer denominator))
      (ratification-error rational "A rational must optionally start with + or -, followed by one or two integers separated by a forward slash.")))

(define-test float (float)
  (or (cl-ppcre:register-groups-bind (base NIL fraction NIL exponent) ("^[+-]?(.*)\\.(.*)?([eE](.*))?$" float)
        (when fraction
          (test-unsigned-integer fraction))
        (when exponent
          (test-unsigned-integer exponent))
        (test-unsigned-integer base))
      (ratification-error float "A float must optionally start with + or - followed by an integer a dot and an integer for the fraction and optionally an 'e' and an integer for the exponent.")))

(define-test real (real)
  (or (rational-p real)
      (float-p real)
      (ratification-error real "A real must be either a rational or a float.")))

(define-test complex (complex)
  (or (cl-ppcre:register-groups-bind (real NIL imag) ("^(.*)[cC](.*)$" complex)
        (test-real real)
        (test-real imag))
      (ratification-error complex "A complex number must be composed of two reals separated by a 'C'.")))

(define-test number (number)
  (or (real-p number)
      (complex-p number)
      (ratification-error number "A number must be either a real or a complex.")))

(define-test boolean (boolean)
  (unless (find boolean '("1" "0" "true" "false" "T" "NIL") :test #'string-equal)
    (ratification-error boolean "A boolean must be one of 1 0 true false T NIL.")))

(define-test character (character)
  (unless (= 1 (length character))
    (ratification-error character "A character must be exactly one character long.")))

(define-test string (string)
  (when (= 0 (length string))
    (ratification-error string "A string must be made up of one character or more.")))

(define-test alphabetic (alpha)
  (loop for char across alpha
        unless (true-alpha-p char)
          do (ratification-error alpha "Invalid character ~a. Only alphabetic characters (a-z A-Z) are allowed." char)))

(define-test numeric (number)
  (loop for char across number
        unless (char<= #\0 char #\9)
          do (ratification-error number "Invalid character ~a. Only numeric characters (0-9) are allowed." char)))

(define-test alphanumeric (alpha)
  (loop for char across alpha
        unless (true-alphanumeric-p char)
          do (ratification-error alpha "Invalid character ~a. Only alphanumeric characters (a-z A-Z 0-9) are allowed." char)))
