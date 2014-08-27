#|
 This file is a part of ratify
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.url)

(define-test hostname (hostname)
  "Test a hostname for validity according to http://en.wikipedia.org/wiki/Hostname"
  (unless (<= 1 (length hostname) 255)
    (ratification-error hostname "Hostname must be between 1 and 255 characters long."))
  (loop with lastdot = 0
        for char across hostname
        for i from 0
        unless (or (true-alphanumeric-p char)
                   (find char "-." :test #'char=))
          do (ratification-error hostname "Invalid character ~a. Hostname parts must consist of either alphanumerics or - ." char)
        when (char= char #\.)
          do (unless (<= 1 (- i lastdot) 63)
               (ratification-error hostname "Hostname parts must be between 1 and 63 characters long."))
             (setf lastdot i)
        finally (unless (<= 1 (- i lastdot) 63)
                  (ratification-error hostname "Hostname parts must be between 1 and 63 characters long."))))

(define-test domain (domain)
  (or (and (char= (aref domain 0) #\[)
           (char= (aref domain (1- (length domain))) #\])
           (test-ip (subseq domain 1 (1- (length domain)))))
      (test-hostname domain)))

(defvar *permitted-protocols* '("ftp" "http" "https"))
(define-test protocol (protocol)
  (find protocol *permitted-protocols* :test #'string-equal))

(define-test url (url)
  (or
   (cl-ppcre:register-groups-bind (NIL protocol domain NIL query NIL fragment) ("^(([^:]+):)?([^\\?]+)(\\?([^#]*))?(\\#(.*))?$" url)
     (when protocol (test-protocol protocol))
     (when domain (test-domain domain))
     (when query (test-query query))
     (when fragment (test-fragment fragment)))
   (ratification-error url "an URL must at the very least consist of a path.")))
