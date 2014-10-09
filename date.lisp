#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.date)

;; According to http://tools.ietf.org/html/rfc3339
;; We make the special exception that the date/time numbers
;; do not have to contain a leading zero, and the T/Z
;; splitters need to be in uppercase.

(define-test year (year)
  "Tests for a valid year. 

[0-9]{4}"
  (unless (= 4 (length year))
    (ratification-error year "Year must be a 4-digit integer."))
  (loop for char across year
        unless (char<= #\0 char #\9)
          do (ratification-error year "Character ~a is not a digit." char)))

(define-parser year (year)
  "Parses the year into an integer."
  (parse-integer year))

(define-test month (month)
  "Tests for a valid month.

[0-9]{1,2}
1<=val<=12"
  (let ((month (ignore-errors (parse-integer month))))
    (unless month
      (ratification-error month "Month must be an integer."))
    (unless (<= 1 month 12)
      (ratification-error month "Month must be an integer between 1 and 12."))))

(define-parser month (month)
  "Parses the month into an integer."
  (parse-integer month))

(define-test day (day)
  "Tests for a valid day.

[0-9]{1,2}
1<=val<=31"
  (let ((day (ignore-errors (parse-integer day))))
    (unless day
      (ratification-error day "Day must be an integer."))
    (unless (<= 1 day 31)
      (ratification-error day "Day must be an integer between 1 and 31."))))

(define-parser day (day)
  "Parses the day into an integer"
  (parse-integer day))

(define-test hour (hour)
  "Tests for a valid hour.

[0-9]{1,2}
0<=val<=23"
  (let ((hour (ignore-errors (parse-integer hour))))
    (unless hour
      (ratification-error hour "Hour must be an integer."))
    (unless (<= 0 hour 23)
      (ratification-error hour "Hour must be an integer between 0 and 23."))))

(define-parser hour (hour)
  "Parses the hour into an integer"
  (parse-integer hour))

(define-test minute (minute)
  "Tests for a valid minute.

[0-9]{1,2}
0<=val<=59"
  (let ((minute (ignore-errors (parse-integer minute))))
    (unless minute
      (ratification-error minute "Minute must be an integer."))
    (unless (<= 0 minute 59)
      (ratification-error minute "Minute must be an integer between 0 and 59."))))

(define-parser minute (minute)
  "Parses the minute into an integer"
  (parse-integer minute))

(define-test second (second)
  "Tests for a valid second.

[0-9]{1,2}
0<=val<=59"
  (let ((second (ignore-errors (parse-integer second))))
    (unless second
      (ratification-error second "Second must be an integer."))
    (unless (<= 0 second 59)
      (ratification-error second "Second must be an integer between 0 and 59."))))

(define-parser second (second)
  "Parses the second into an integer"
  (parse-integer second))

(define-test offset (offset)
  "Tests for a valid offset.

[-+]hour:minute"
  (when (= 0 (length offset))
    (ratification-error offset "Offset must be composed of +/-hours:minutes ."))
  (unless (or (char= #\- (aref offset 0))
              (char= #\+ (aref offset 0)))
    (ratification-error offset "Offset must begin with either + or - ."))
  (or (cl-ppcre:register-groups-bind (hour minute) ("^[-+]([^:]+):([^:]+)$" offset)
        (test-hour hour)
        (test-minute minute))
      (ratification-error offset "Offset must specify hours and minutes.")))

(define-parser offset (offset)
  "Parses the offset into a list of (DIR HOUR MINUTE), wherein DIR is 
a string of either \"+\" or \"-\", denoting the direction of the offset. "
  (cl-ppcre:register-groups-bind (dir hour minute) ("^([-+])([^:]+):([^:]+)$" offset)
    (list
     dir
     (parse-integer (or hour ""))
     (parse-integer (or minute "")))))

(define-test time (time)
  "Tests for a valid time.

<hour>:<minute>:<second>Z<offset>"
  (or (cl-ppcre:register-groups-bind (hour minute second NIL offset) ("^([^:]+):([^:]+):([^Z]+)(Z(.+))?$" time)
        (when offset
          (test-offset offset))
        (test-hour hour)
        (test-minute minute)
        (test-second second))
      (ratification-error time "Time must be made up of hour:minute:second followed by an optional offset: Z+hours:minutes .")))

(define-parser time (time)
  "Parses the given time into a LOCAL-TIME:TIMESTAMP object."
  (local-time:parse-timestring time :allow-missing-date-part T :allow-missing-time-part NIL :allow-missing-timezone-part T))

(define-test date (date)
  "Tests for a valid date.

<year>-<month>-<day>"
  (let ((parts (cl-ppcre:split "-" date)))
    (unless (= 3 (length parts))
      (ratification-error date "Date must be made up of year-month-day ."))
    (destructuring-bind (year month day) parts
      (test-year year)
      (test-month month)
      (test-day day))))

(define-parser date (date)
  "Parses the given date into a LOCAL-TIME:TIMESTAMP object."
  (local-time:parse-timestring date :allow-missing-date-part NIL :allow-missing-time-part T :allow-missing-timezone-part T))

(define-test datetime (datetime)
  "Tests for a valid datetime.

<year>-<month>-<day>T<hour>:<minute>:<second>Z<offset>"
  (let ((parts (cl-ppcre:split "T" datetime)))
    (unless (<= 1 (length parts) 2)
      (ratification-error datetime "Datetime must specify at least the date and at most date and time separated by T."))
    (test-date (first parts))
    (when (second parts)
      (test-time (second parts)))))

(define-parser datetime (datetime)
  "Parses the given datetime into a LOCAL-TIME:TIMESTAMP object.
The only part that is allowed to be omitted is the timezone offset specification."
  (local-time:parse-timestring datetime :allow-missing-date-part NIL :allow-missing-time-part NIL :allow-missing-timezone-part T))
