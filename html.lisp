#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.html)

(define-test checkbox (input)
  (unless (or (not input) (stringp input))
    (ratification-error input "Must be a string or NIL.")))

;; color

;; date

;; datetime

(define-test datetime-local (datetime)
  (test-datetime datetime))

;; email

(define-test file (file)
  (unless (pathnamep file)
    (ratification-error file "Not a file.")))

;; month

;; number

(define-test password (pw)
  (test-text pw))

(define-test radio (radio)
  (test-checkbox radio))

(define-test range (range)
  (test-float range))

(define-test search (search)
  (test-text search))

(define-test tel (tel)
  ;; We can't do better than this. Curse internationality.
  (test-text tel))

(define-test text (text)
  (when (find #\Newline text)
    (ratification-error text "Text must be a single line.")))

(define-test textarea (text)
  text)

;; time

;; url

;; week
