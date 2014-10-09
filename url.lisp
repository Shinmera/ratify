#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.url)

(define-test hostname (hostname)
  "Test a hostname for validity according to http://en.wikipedia.org/wiki/Hostname

[a-zA-Z0-9-]{1,63}(\\.[a-zA-Z0-9-]{1,63})*
1<=length<=255"
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
  "Tests for a valid domain.

\[<ip>\]|<hostname>"
  (or (and (char= (aref domain 0) #\[)
           (char= (aref domain (1- (length domain))) #\])
           (test-ip (subseq domain 1 (1- (length domain)))))
      (test-hostname domain)))

(defvar *permitted-protocols* '("ftp" "http" "https")
  "List of permitted protocols in a URL.")

(define-test protocol (protocol)
  "Tests for a valid protocol according to *PERMITTED-PROTOCOLS*"
  (find protocol *permitted-protocols* :test #'string-equal))

(define-test url (url)
  "Tests for a valid URL.

 (<protocol>://)?(<domain>)?<absolute-path>(\?<query>)?(#<fragment>)?"
  (or
   (cl-ppcre:register-groups-bind (NIL protocol domain path NIL query NIL fragment) ("^(([^:]+)://)?([^/]+)?(/[^\\?]+)(\\?([^#]*))?(\\#(.*))?$" url)
     (when protocol (test-protocol protocol))
     (when domain (test-domain domain))
     (when path (test-absolute-path path))
     (when query (test-query query))
     (when fragment (test-fragment fragment)))
   (ratification-error url "an URL must at the very least consist of a path.")))
