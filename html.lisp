#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.html)

(define-test checkbox (input start end)
  (unless (or (not input) (stringp input))
    (ratification-error input "Must be a string or NIL.")))

;; color

;; date

;; datetime

(define-test datetime-local (datetime start end)
  (test-datetime datetime start end))

;; email

(define-test file (file start end)
  (unless (pathnamep file)
    (ratification-error file "Not a file.")))

;; month

;; number

(define-test password (pw start end)
  (test-text pw start end))

(define-test radio (radio start end)
  (test-checkbox radio start end))

(define-test range (range start end)
  (test-float range start end))

(define-test search (search start end)
  (test-text search start end))

(define-test tel (tel start end)
  ;; We can't do better than this. Curse internationality.
  (test-text tel start end))

(define-test text (text start end)
  (when (find #\Newline text :start start :end end)
    (ratification-error text "Text must be a single line.")))

(define-test textarea (text start end)
  text)

;; time

;; url

;; week
