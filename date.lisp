#|
 This file is a part of ratify
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.date)

;; According to http://tools.ietf.org/html/rfc3339
;; We make the special exception that the date/time numbers
;; do not have to contain a leading zero, since parsing will go
;; fine regardless.

(define-test year (year)
  (unless (= 4 (length year))
    (ratification-error year "Year must be a 4-digit integer."))
  (loop for char across year
        unless (char<= #\0 char #\9)
          do (ratification-error year "Character ~a is not a digit." char)))

(define-test month (month)
  (let ((month (ignore-errors (parse-integer month))))
    (unless month
      (ratification-error month "Month must be an integer."))
    (unless (<= 1 month 12)
      (ratification-error month "Month must be an integer between 1 and 12."))))

(define-test day (day)
  (let ((day (ignore-errors (parse-integer day))))
    (unless day
      (ratification-error day "Day must be an integer."))
    (unless (<= 1 day 31)
      (ratification-error day "Day must be an integer between 1 and 31."))))

(define-test hour (hour)
  (let ((hour (ignore-errors (parse-integer hour))))
    (unless hour
      (ratification-error hour "Hour must be an integer."))
    (unless (<= 0 hour 23)
      (ratification-error hour "Hour must be an integer between 0 and 23."))))

(define-test minute (minute)
  (let ((minute (ignore-errors (parse-integer minute))))
    (unless minute
      (ratification-error minute "Minute must be an integer."))
    (unless (<= 0 minute 59)
      (ratification-error minute "Minute must be an integer between 0 and 59."))))

(define-test second (second)
  (let ((second (ignore-errors (parse-integer second))))
    (unless second
      (ratification-error second "Second must be an integer."))
    (unless (<= 0 second 59)
      (ratification-error second "Second must be an integer between 0 and 59."))))

(define-test offset (offset)
  (when (= 0 (length offset))
    (ratification-error offset "Offset must be composed of +/-hours:minutes ."))
  (unless (or (char= #\- (aref offset 0))
              (char= #\+ (aref offset 0)))
    (ratification-error offset "Offset must begin with either + or - ."))
  (or (cl-ppcre:register-groups-bind (hour minute) ("^[-+]([^:]+):([^:]+)$" offset)
        (test-hour hour)
        (test-minute minute))
      (ratification-error offset "Offset must specify hours and minutes.")))

(define-test time (time)
  (or (cl-ppcre:register-groups-bind (hour minute second NIL offset) ("^([^:]+):([^:]+):([^Zz]+)([Zz](.+))?$" time)
        (when offset
          (test-offset offset))
        (test-hour hour)
        (test-minute minute)
        (test-second second))
   (ratification-error time "Time must be made up of hour:minute:second followed by an optional offset: Z+hours:minutes .")))

(define-test date (date)
  (let ((parts (cl-ppcre:split "-" date)))
    (unless (= 3 (length parts))
      (ratification-error date "Date must be made up of year-month-day ."))
    (destructuring-bind (year month day) parts
      (test-year year)
      (test-month month)
      (test-day day))))

(define-test datetime (datetime)
  (let ((parts (cl-ppcre:split "[tT]" datetime)))
    (unless (<= 1 (length parts) 2)
      (ratification-error datetime "Datetime must specify at least the date and at most date and time separated by T."))
    (test-date (first parts))
    (when (second parts)
      (test-time (second parts)))))
