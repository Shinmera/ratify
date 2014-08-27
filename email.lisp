#|
 This file is a part of ratify
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.email)

(defun split-email (email)
  (let ((atpos (position #\@ email)))
    (unless atpos (ratification-error email "No @ found."))
    (cons (subseq email 0 atpos)
          (subseq email (1+ atpos)))))

(define-test local-part (local-part)
  (let ((length (length local-part)))
    (unless (<= 1 length 64)
      (ratification-error local-part "Local-part of an email must be between 1 and 64 characters long."))
    (loop for char across local-part
          for i from 0
          ;; Caveat: We hope the implementation uses ascii or unicode.
          ;; We cannot use alpha-char-p since depending on implementation characters like ü pass as well.
          unless (or (true-alphanumeric-p char)
                     ;; dot, but not at start or end
                     (and (char= char #\.)
                          (/= i 0)
                          (/= i (1- length)))
                     ;; Special characters
                     (find char "!#$%&'*+-/=?^_`{|}~" :test #'char=)
                     #+(or sb-unicode unicode)
                     (char> char #\Rubout))
            do (ratification-error local-part
                                   #+(or sb-unicode unicode) "~a is not a valid character. Permitted are a-z A-Z 0-9 . ! # $ % & ' * + - / = ? ^ _ ` { | } ~~ or unicode characters."
                                   #-(or sb-unicode unicode) "~a is not a valid character. Permitted are a-z A-Z 0-9 . ! # $ % & ' * + - / = ? ^ _ ` { | } ~~"
                                   char))))

(define-test email (email)
  "Test an e-mail address for validity according to http://en.wikipedia.org/wiki/Email_address#Syntax"
  (let ((split (split-email email)))
    (test-local-part (car split))
    (test-domain (cdr split))))
