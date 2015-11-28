#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.email)

(defun email-atpos (email start end)
  (let ((atpos (position #\@ email :start start :end end)))
    (unless atpos (ratification-error email "No @ found."))
    atpos))

(define-test local-part (local-part start end)
  "Tests for a valid email local-part.

[!#$%&'*+-/=?^_`{|}~a-zA-Z0-9][!#$%&'*+-/=?^_`{|}~.a-zA-Z0-9]{0,63}"
  (let ((length (- end start)))
    (unless (<= 1 length 64)
      (ratification-error local-part "Local-part of an email must be between 1 and 64 characters long."))
    (loop for i from start below end
          for char = (char local-part i)
          ;; Caveat: We hope the implementation uses ascii or unicode.
          ;; We cannot use alpha-char-p since depending on implementation characters like ü pass as well.
          do (unless (or (true-alphanumeric-p char)
                         ;; dot, but not at start or end
                         (and (char= char #\.)
                              (/= i start)
                              (/= i (1- end)))
                         ;; Special characters
                         (find char "!#$%&'*+-/=?^_`{|}~" :test #'char=)
                         #+(or sb-unicode unicode)
                         (char> char #\Rubout))
               (ratification-error local-part
                                   #+(or sb-unicode unicode) "~a is not a valid character. Permitted are a-z A-Z 0-9 . ! # $ % & ' * + - / = ? ^ _ ` { | } ~~ or unicode characters."
                                   #-(or sb-unicode unicode) "~a is not a valid character. Permitted are a-z A-Z 0-9 . ! # $ % & ' * + - / = ? ^ _ ` { | } ~~"
                                   char)))))

(define-test email (email start end)
  "Test an e-mail address for validity according to http://en.wikipedia.org/wiki/Email_address#Syntax

<local-part>@<domain>"
  (let ((atpos (email-atpos email start end)))
    (test-local-part email 0 atpos)
    (test-domain email (1+ atpos) end)))
