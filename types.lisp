#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.types)

(define-test bit (bit)
  "Tests for a valid bit.

[01]"
  (unless (or (string= bit "1")
              (string= bit "0"))
    (ratification-error bit "A bit must be either 0 or 1.")))

(define-parser bit (bit)
  "Parses into a bit of either 1 or 0."
  (the bit (if (string= bit "1") 1 0)))

(define-test unsigned-integer (integer)
  "Tests for a valid unsigned integer.

<numeric>"
  (when (= 0 (length integer))
    (ratification-error integer "An integer must be at least one digit."))
  (test-numeric integer))

(define-parser unsigned-integer (integer)
  "Parses into an integer."
  (the integer (parse-integer integer)))

(define-test integer (integer)
  "Tests for a valid signed integer.

[+-]?<unsigned-integer>"
  (or (cl-ppcre:register-groups-bind (integer) ("^[+-]?(.+)$" integer)
        (test-unsigned-integer integer))
      (ratification-error integer "An integer must be a number optionally preceded by + or -.")))

(setf (parser 'integer) #'parse-integer)

(define-test ratio (ratio)
  "Tests for a valid ratio.

[+-]?<unsigned-integer>/<unsigned-integer>"
  (or (cl-ppcre:register-groups-bind (numerator denominator) ("^[+-]?(.+)/(.+)$" ratio)
        (test-unsigned-integer numerator)
        (test-unsigned-integer denominator))
      (ratification-error ratio "A ratio must optionally start with + or -, followed by two integers separated by a forward slash.")))

(define-parser ratio (ratio)
  "Parses into a ratio."
  (cl-ppcre:register-groups-bind (numerator denominator) ("^(.+)/(.+)$" ratio)
    (the ratio (/ (parse-integer (or numerator "")) ; It can't be NIL, but SBCL throws gross warnings otherwise.
                  (parse-integer (or denominator ""))))))

(define-test rational (rational)
  "Tests for a valid rational.

[+-]?<unsigned-integer>(/<unsigned-integer>)?"
  (or (cl-ppcre:register-groups-bind (numerator NIL denominator) ("^[+-]?(.+?)(/(.+))?$" rational)
        (when denominator
          (test-unsigned-integer denominator))
        (test-unsigned-integer numerator))
      (ratification-error rational "A rational must optionally start with + or -, followed by one or two integers separated by a forward slash.")))

(define-parser rational (rational)
  "Parses into a rational."
  (cl-ppcre:register-groups-bind (numerator NIL denominator) ("^(.+?)(/(.+))?$" rational)
    (the rational
         (/ (parse-integer (or numerator ""))
            (if denominator (parse-integer denominator) 1)))))

(define-test float (float)
  "Tests for a valid float.

[+-]?<unsigned-integer>(\\.<unsigned-integer>)?(e<unsigned-integer>)?"
  (or (cl-ppcre:register-groups-bind (base NIL fraction NIL exponent) ("^[+-]?(.+?)(\\.(.+))?(e(.+))?$" float)
        (when fraction
          (test-unsigned-integer fraction))
        (when exponent
          (test-unsigned-integer exponent))
        (test-unsigned-integer base))
      (ratification-error float "A float must optionally start with + or - followed by an integer a dot and an integer for the fraction and optionally an 'e' and an integer for the exponent.")))

(define-parser float (float)
  "Parses into a float."
  (the float
       (parse-float:parse-float float)))

(define-test real (real)
  "Tests for a valid real.

<rational>|<float>"
  (or (rational-p real)
      (float-p real)
      (ratification-error real "A real must be either a rational or a float.")))

(define-parser real (real)
  "Parses into a real."
  (the real
       (if (find #\. real)
           (parse-float real)
           (parse-rational real))))

(define-test complex (complex)
  "Tests for a valid complex number.

<real>[cC]<real>"
  (or (cl-ppcre:register-groups-bind (real imag) ("^(.+)[cC](.+)$" complex)
        (test-real real)
        (test-real imag))
      (ratification-error complex "A complex number must be composed of two reals separated by a 'C'.")))

(define-parser complex (complex)
  "Parses into a complex number."
  (the complex
       (or (cl-ppcre:register-groups-bind (real imag) ("^(.+)[cC](.+)$" complex)
             (complex (parse-real real)
                      (parse-real imag)))
           (error "Failed to parse, not a valid complex."))))

(define-test number (number)
  "Tests for a valid number.

<real>|<complex>"
  (or (real-p number)
      (complex-p number)
      (ratification-error number "A number must be either a real or a complex.")))

(define-parser number (number)
  "Parses into a number."
  (the number
       (or (cl-ppcre:register-groups-bind (real NIL imag) ("^(.+?)([cC](.+))?$" number)
             (if imag
                 (complex (parse-real real) (parse-real imag))
                 (parse-real real)))
           (error "Failed to parse, not a valid number."))))

(define-test boolean (boolean)
  "Tests for a valid boolean.

1|0|true|false|T|NIL
case-insensitive"
  (unless (find boolean '("1" "0" "true" "false" "T" "NIL") :test #'string-equal)
    (ratification-error boolean "A boolean must be one of 1 0 true false T NIL.")))

(define-parser boolean (boolean)
  "Parses into a boolean.

Returns T if one of (\"1\" \"true\" \"T\"), NIL otherwise."
  (the boolean
       (if (find boolean '("1" "true" "T") :test #'string-equal)
           T
           NIL)))

(define-test character (character)
  "Tests for a valid character.

.{1}"
  (unless (= 1 (length character))
    (ratification-error character "A character must be exactly one character long.")))

(define-parser character (character)
  "Parses into a character."
  (the character
       (aref character 0)))

(define-test string (string)
  "Tests for a valid string.

.+"
  (when (= 0 (length string))
    (ratification-error string "A string must be made up of one character or more.")))

(define-parser string (string)
  "Parses into a string (simply returns its argument)."
  (the string
       string))

(define-test alphabetic (alpha)
  "Tests for an alphabetic string.

[a-zA-Z]*"
  (loop for char across alpha
        unless (true-alpha-p char)
          do (ratification-error alpha "Invalid character ~a. Only alphabetic characters (a-z A-Z) are allowed." char)))

(define-test numeric (number)
  "Tests for a numeric string.

[0-9]*"
  (loop for char across number
        unless (char<= #\0 char #\9)
          do (ratification-error number "Invalid character ~a. Only numeric characters (0-9) are allowed." char)))

(define-test alphanumeric (alpha)
  "Tests for an alphanumeric string.

[a-zA-Z0-9]*"
  (loop for char across alpha
        unless (true-alphanumeric-p char)
          do (ratification-error alpha "Invalid character ~a. Only alphanumeric characters (a-z A-Z 0-9) are allowed." char)))
