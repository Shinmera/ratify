#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.url)

(define-test hostname (hostname start end)
  "Test a hostname for validity according to http://en.wikipedia.org/wiki/Hostname

[a-zA-Z0-9-]{1,63}(\\.[a-zA-Z0-9-]{1,63})*
1<=length<=255"
  (unless (<= 1 (- end start) 255)
    (ratification-error hostname "Hostname must be between 1 and 255 characters long."))
  (loop with lastdot = start
        for i from start below end
        for char = (char hostname i)
        do (unless (or (true-alphanumeric-p char)
                       (find char "-." :test #'char=))
             (ratification-error hostname "Invalid character ~a. Hostname parts must consist of either alphanumerics or - ." char))
           (when (char= char #\.)
             (unless (<= 1 (- i lastdot) 63)
               (ratification-error hostname "Hostname parts must be between 1 and 63 characters long."))
             (setf lastdot i))
        finally (unless (<= 1 (- i lastdot) 63)
                  (ratification-error hostname "Hostname parts must be between 1 and 63 characters long."))))

(define-test domain (domain start end)
  "Tests for a valid domain.

\[<ip>\]|<hostname>"
  (or (and (char= (aref domain start) #\[)
           (char= (aref domain (1- end)) #\])
           (test-ip (subseq domain (1+ start) (1- end))))
      (test-hostname domain start end)))

(defvar *permitted-protocols* '("ftp" "http" "https")
  "List of permitted protocols in a URL.")

(define-test protocol (protocol start end)
  "Tests for a valid protocol according to *PERMITTED-PROTOCOLS*"
  (find protocol *permitted-protocols* :test (lambda (a b) (string-equal a b :start1 start :end1 end))))

(define-test url (url start end)
  "Tests for a valid URL.

 (<protocol>://)?(<domain>)?<absolute-path>(\?<query>)?(#<fragment>)?"
  (or
   (cl-ppcre:register-groups-bind (NIL protocol domain path NIL query NIL fragment) ("^(([^:]+):\\/\\/)?([^/]+)?(\\/[^\\?]*)(\\?([^#]*))?(#(.*))?$" url :start start :end end)
     (when protocol (test-protocol protocol))
     (when domain (test-domain domain))
     (when path (test-absolute-path path))
     (when query (test-query query))
     (when fragment (test-fragment fragment))
     T)
   (ratification-error url "An URL must at the very least consist of a path.")))
