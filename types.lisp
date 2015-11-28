#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.types)

(define-test bit (bit start end)
  "Tests for a valid bit.

[01]"
  (unless (or (string= bit "1" :start1 start :end1 end)
              (string= bit "0" :start1 start :end1 end))
    (ratification-error bit "A bit must be either 0 or 1.")))

(define-parser bit (bit start end)
  "Parses into a bit of either 1 or 0."
  (if (string= bit "1" :start1 start :end1 end) 1 0))

(define-test unsigned-integer (integer start end)
  "Tests for a valid unsigned integer.

<numeric>"
  (when (= 0 (- end start))
    (ratification-error integer "An integer must be at least one digit."))
  (test-numeric integer start end))

(define-parser unsigned-integer (integer start end)
  "Parses into an integer."
  (parse-integer integer :start start :end end))

(define-test integer (integer start end)
  "Tests for a valid signed integer.

[+-]?<unsigned-integer>"
  (or (cl-ppcre:register-groups-bind (integer) ("^[+-]?(.+)$" integer :start start :end end)
        (test-unsigned-integer integer))
      (ratification-error integer "An integer must be a number optionally preceded by + or -.")))

(setf (parser 'integer) #'parse-unsigned-integer)

(define-test ratio (ratio start end)
  "Tests for a valid ratio.

[+-]?<unsigned-integer>/<unsigned-integer>"
  (or (cl-ppcre:register-groups-bind (numerator denominator) ("^[+-]?(.+)/(.+)$" ratio :start start :end end)
        (test-unsigned-integer numerator)
        (test-unsigned-integer denominator))
      (ratification-error ratio "A ratio must optionally start with + or -, followed by two integers separated by a forward slash.")))

(define-parser ratio (ratio start end)
  "Parses into a ratio."
  (cl-ppcre:register-groups-bind (numerator denominator) ("^(.+)/(.+)$" ratio :start start :end end)
    (/ (parse-integer (or numerator "")) ; It can't be NIL, but SBCL throws gross warnings otherwise.
       (parse-integer (or denominator "")))))

(define-test rational (rational start end)
  "Tests for a valid rational.

[+-]?<unsigned-integer>(/<unsigned-integer>)?"
  (or (cl-ppcre:register-groups-bind (numerator NIL denominator) ("^[+-]?(.+?)(/(.+))?$" rational :start start :end end)
        (when denominator
          (test-unsigned-integer denominator))
        (test-unsigned-integer numerator))
      (ratification-error rational "A rational must optionally start with + or -, followed by one or two integers separated by a forward slash.")))

(define-parser rational (rational start end)
  "Parses into a rational."
  (cl-ppcre:register-groups-bind (numerator NIL denominator) ("^(.+?)(/(.+))?$" rational :start start :end end)
    (/ (parse-integer (or numerator ""))
       (if denominator (parse-integer denominator) 1))))

(define-test float (float start end)
  "Tests for a valid float.

[+-]?<unsigned-integer>(\\.<unsigned-integer>)?(e<unsigned-integer>)?"
  (or (cl-ppcre:register-groups-bind (base NIL fraction NIL exponent) ("^[+-]?(.+?)(\\.(.+))?(e(.+))?$" float :start start :end end)
        (when fraction
          (test-unsigned-integer fraction))
        (when exponent
          (test-unsigned-integer exponent))
        (test-unsigned-integer base))
      (ratification-error float "A float must optionally start with + or - followed by an integer a dot and an integer for the fraction and optionally an 'e' and an integer for the exponent.")))

(define-parser float (float start end)
  "Parses into a float."
  (parse-float:parse-float float :start start :end end))

(define-test real (real start end)
  "Tests for a valid real.

<rational>|<float>"
  (or (rational-p real start end)
      (float-p real start end)
      (ratification-error real "A real must be either a rational or a float.")))

(define-parser real (real start end)
  "Parses into a real."
  (if (find #\. real :start start :end end)
      (parse-float real start end)
      (parse-rational real start end)))

(define-test complex (complex start end)
  "Tests for a valid complex number.

<real>[cC]<real>"
  (or (cl-ppcre:register-groups-bind (real imag) ("^(.+)[cC](.+)$" complex :start start :end end)
        (test-real real)
        (test-real imag))
      (ratification-error complex "A complex number must be composed of two reals separated by a 'C'.")))

(define-parser complex (complex start end)
  "Parses into a complex number."
  (or (cl-ppcre:register-groups-bind (real imag) ("^(.+)[cC](.+)$" complex :start start :end end)
        (complex (parse-real real)
                 (parse-real imag)))
      (error "Failed to parse, not a valid complex.")))

(define-test number (number start end)
  "Tests for a valid number.

<real>|<complex>"
  (or (real-p number)
      (complex-p number)
      (ratification-error number "A number must be either a real or a complex.")))

(define-parser number (number start end)
  "Parses into a number."
  (or (cl-ppcre:register-groups-bind (real NIL imag) ("^(.+?)([cC](.+))?$" number :start start :end end)
        (if imag
            (complex (parse-real real) (parse-real imag))
            (parse-real real)))
      (error "Failed to parse, not a valid number.")))

(define-test boolean (boolean start end)
  "Tests for a valid boolean.

1|0|true|false|T|NIL
case-insensitive"
  (unless (find boolean '("1" "0" "true" "false" "T" "NIL") :test (lambda (a b) (string-equal a b :start1 start :end1 end)))
    (ratification-error boolean "A boolean must be one of 1 0 true false T NIL.")))

(define-parser boolean (boolean start end)
  "Parses into a boolean.

Returns T if one of (\"1\" \"true\" \"T\"), NIL otherwise."
  (if (find boolean '("1" "true" "T") :test #'(lambda (a b) (string-equal a b :start1 start :end1 end)))
      T
      NIL))

(define-test character (character start end)
  "Tests for a valid character.

.{1}"
  (unless (= 1 (- end start))
    (ratification-error character "A character must be exactly one character long.")))

(define-parser character (character start end)
  "Parses into a character."
  (char character start))

(define-test string (string start end)
  "Tests for a valid string.

.+"
  (when (= 0 (- end start))
    (ratification-error string "A string must be made up of one character or more.")))

(define-parser string (string start end)
  "Parses into a string (simply returns its argument)."
  string)

(define-test alphabetic (alpha start end)
  "Tests for an alphabetic string.

[a-zA-Z]*"
  (loop for i from start below end
        for char = (char alpha i)
        do (unless (true-alpha-p char)
             (ratification-error alpha "Invalid character ~a. Only alphabetic characters (a-z A-Z) are allowed." char))))

(define-test numeric (number start end)
  "Tests for a numeric string.

[0-9]*"
  (loop for i from start below end
        for char = (char number i)
        do (unless (char<= #\0 char #\9)
             (ratification-error number "Invalid character ~a. Only numeric characters (0-9) are allowed." char))))

(define-test alphanumeric (alpha start end)
  "Tests for an alphanumeric string.

[a-zA-Z0-9]*"
  (loop for i from start below end
        for char = (char alpha i)
        do (unless (true-alphanumeric-p char)
             (ratification-error alpha "Invalid character ~a. Only alphanumeric characters (a-z A-Z 0-9) are allowed." char))))
