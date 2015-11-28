#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.ratify.uri)

;; According to http://tools.ietf.org/html/rfc3986

(defun general-delimiter-p (char)
  (find char ":/?#[]@" :test #'char=))

(defun sub-delimiter-p (char)
  (find char "!$&'()*+,;=" :test #'char=))

(defun reserved-character-p (char)
  (or (general-delimiter-p char)
      (sub-delimiter-p char)))

(defun unreserved-character-p (char)
  (or (true-alphanumeric-p char)
      (find char "-._~" :test #'char=)))

(defun percent-encoded-p (char)
  ;; I know this isn't right, but the alternative is oh so much more painful.
  ;; we'll delegate the proper resolving of urlencoded chars to another lib.
  (char= char #\%))

(defun pchar-p (char)
  (or (unreserved-character-p char)
      (sub-delimiter-p char)
      (percent-encoded-p char)
      (find char ":@" :test #'char=)))

(define-test ipv4 (ip start end)
  "Tests for a valid IPv4

<unsigned-integer>\\.<unsigned-integer>\\.<unsigned-integer>\\.<unsigned-integer>
0<=unsigned-integer<=255"
  (let ((parts (cl-ppcre:split "\\." ip :limit 5 :start start :end end)))
    (unless (= (length parts) 4)
      (ratification-error ip "IPv4 addresses must consist of four parts."))
    (loop for part in parts
          for num = (ignore-errors (parse-integer part))
          do (unless (and num (<= 0 num 255))
               (ratification-error ip "~s is not a decimal integer between 0 and 255." part)))))

(define-test ipv6 (ip start end)
  "Tests for a valid IPv6

[0-9A-F]:(:|[0-9A-F]:){1,6})[0-9A-F]?
0000<=val<=FFFF"
  (let ((parts (cl-ppcre:split ":" ip :limit 9 :start start :end end)))
    (unless (<= (length parts) 8)
      (ratification-error ip "IPv6 must consist of 8 or less parts."))
    (loop for part in parts
          for num = (ignore-errors (parse-integer part :radix 16))
          for i from 0
          do (unless (or (not num) (<= #x0000 num #xFFFF))
               (ratification-error ip "~s is not a hexadecimal integer between 0 and FFFF." part)))))

(define-test ip (ip start end)
  "Tests for a valid IP address.

<ipv6>|<ipv4>"
  (cond ((find #\: ip :start start :end end)
         (test-ipv6 ip start end))
        ((find #\. ip :start start :end end)
         (test-ipv4 ip start end))
        (T (ratification-error ip "This is neither an IPv4 nor an IPv6."))))

(define-test host (host start end)
  "Tests for a valid host name.

\[<ip>\]|[a-zA-Z0-9-._~%!$&'()*+,;=]+"
  (when (= 0 (- end start))
    (ratification-error host "Host must be at least one character long."))
  (when (and (char= (aref host start) #\[)
             (char= (aref host (1- end)) #\]))
    (test-ip host (1+ start) (- end 2)))
  (loop for i from start below end
        for char = (char host i)
        do (unless (or (unreserved-character-p char)
                       (percent-encoded-p char)
                       (sub-delimiter-p char))
             (ratification-error host "Invalid character ~a. Host can only contain alphanumerics or - . _ ~~ % ! $ & ' ( ) * + , ; =" char))))

(define-test scheme (scheme start end)
  "Tests for a valid scheme.

[a-zA-Z][a-zA-Z0-9-.+]*"
  (unless (< 0 (- end start))
    (ratification-error scheme "A scheme must be at least one character long."))
  (unless (true-alpha-p (char scheme start))
    (ratification-error scheme "Scheme must start with an alphabetic character."))
  (loop for i from (1+ start) below end
        for char = (char scheme i)
        do (unless (or (true-alphanumeric-p char)
                       (find char "-.+" :test #'char=))
             (ratification-error scheme "Invalid character ~a. Scheme can only contain alphanumerics or - . +" char))))

(define-test user (user start end)
  "Tests for a valid user.

[a-zA-Z0-9%!$&'()*+,;=-._~:]+"
  (when (= 0 (- end start))
    (ratification-error user "User must be at least one character long."))
  (loop for i from start below end
        for char = (char user i)
        do (unless (or (unreserved-character-p char)
                       (sub-delimiter-p char)
                       (percent-encoded-p char)
                       (char= char #\:))
             (ratification-error user "Invalid character ~a. Username can only contain alphanumerics or % ! $ & ' ( ) * + , ; = - . _ ~~ :" char))))

(define-test port (port start end)
  "Tests for a valid port.

<unsigned-integer>
0<=val<=65535"
  (let ((num (ignore-errors (parse-integer port :start start :end end))))
    (unless num
      (ratification-error port "Port must be a decimal integer."))
    (unless (<= 0 port 65535)
      (ratification-error port "Port must be between 0 and 65535."))))

(define-test authority (authority start end)
  "Tests for a valid authority.

(<user>@)?<authority>(:<port>)?"
  (let ((atpos (position #\@ authority :start start :end end)))
    (when atpos
      (test-user authority start atpos)
      (setf start (1+ atpos))))
  (let ((colonpos (position #\: authority :start start :end end)))
    (when colonpos
      (test-port authority (1+ colonpos))
      (setf end colonpos)))
  (test-host authority start end))

(define-test path-segment (segment start end)
  "Tests for a valid path segment.

[a-zA-Z0-9!$&'()*+,;=-._~:@]+"
  (loop for i from start below end
        for char = (char segment i)
        do (unless (pchar-p char)
             (ratification-error segment "Invalid character ~a. Path segment can only contain alphanumerics or ! $ & ' ( ) * + , ; = - . _ ~~ : @" char))))

(define-test rootless-path (path start end)
  "Tests for a valid rootless path.

<segment-nz>(/<segment>)?"
  (when (= 0 (- end start))
    (ratification-error path "Path must be at least one character long."))
  (loop with begin = start
        for i from start below end
        for char = (char path i)
        do (when (char= char #\/)
             (test-path-segment path begin i)
             (setf begin (1+ i)))
        finally (test-path-segment path begin i)))

(define-test absolute-path (path start end)
  "Tests for a valid absolute path.

/<rootless-path>"
  (unless (< 0 (- end start))
    (ratification-error path "Path must be at least one character long."))
  (unless (char= (char path start) #\/)
    (ratification-error path "An absolute path must start with a forward slash."))
  (when (< 1 (- end start))
    (when (char= (char path (1+ start)) #\/)
      (ratification-error path "Beginning slash must be followed by a non-slash character."))
    (test-rootless-path path (1+ start) end)))

(define-test hierarchical-part (hierarchical start end)
  "Tests for a valid hierarchical part.

<absolute-path>|//<authority><absolute-path>"
  (let ((length (- end start)))
    (when (and (= length 1) (string/= hierarchical "/" :start1 start :end1 end))
      (ratification-error hierarchical "Hierarchical part must be either a path or begin with //."))
    (if (and (< 1 length)
             (string= hierarchical "//" :start1 (1+ start) :end1 (+ start 2)))
        (let ((slashpos (position #\/ hierarchical :start (+ start 2) :end end)))
          (cond (slashpos
                 (test-authority hierarchical (+ start 2) slashpos)
                 (test-absolute-path hierarchical slashpos end))
                (T
                 (test-authority hierarchical (+ start 2) end))))
        (test-absolute-path hierarchical start end))))

(define-test query (query start end)
  "Tests for a valid query part.

[a-zA-Z0-9!$&'()*+,;=-._~:@?/]+"
  (unless (< 0 (- end start))
    (ratification-error query "Query must be at least one character long."))
  (loop for i from start below end
        for char = (char query i)
        do (unless (or (pchar-p char)
                       (find char "?/" :test #'char=))
             (ratification-error query "Invalid character ~a. Query can only contain alphanumercs or ! $ & ' ( ) * + , ; = - . _ ~~ : @ ? /" char))))

(define-test fragment (fragment start end)
  "Tests for a valid fragment part.

[a-zA-Z0-9!$&'()*+,;=-._~:@?/]+"
  (unless (< 0 (- end start))
    (ratification-error fragment "Fragment must be at least one character long."))
  (loop for i from start below end
        for char = (char fragment i)
        do (unless (or (pchar-p char)
                       (find char "?/" :test #'char=))
             (ratification-error fragment "Invalid character ~a. Fragment can only contain alphanumercs or ! $ & ' ( ) * + , ; = - . _ ~~ : @ ? /" char))))

(define-test uri (uri start end)
  "Tests for a valid URI according to http://tools.ietf.org/html/rfc3986

<scheme>:<hierarchical-part>(\?<query>)?(#<fragment>)?"
  (or
   (cl-ppcre:register-groups-bind (scheme hierarchical NIL query NIL fragment) ("^([^:]+):([^\\?]+)(\\?([^#]*))?(\\#(.*))?$" uri :start start :end end)
     (when scheme (test-scheme scheme))
     (when hierarchical (test-hierarchical-part hierarchical))
     (when query (test-query query))
     (when fragment (test-fragment fragment))
     T)
   (ratification-error uri "Uri must consist of at least a scheme followed by a colon and a path.")))
