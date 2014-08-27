#|
 This file is a part of ratify
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
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

(define-test ipv4 (ip)
  (let ((parts (cl-ppcre:split "\\." ip :limit 5)))
    (unless (= (length parts) 4)
      (ratification-error ip "IPv4 addresses must consist of four parts."))
    (loop for part in parts
          for num = (ignore-errors (parse-integer part))
          unless (and num (<= 0 num 255))
            do (ratification-error ip "~s is not a decimal integer between 0 and 255." part))))

(define-test ipv6 (ip)
  (let* ((parts (cl-ppcre:split ":" ip :limit 9))
         (count (length parts)))
    (unless (<= count 8)
      (ratification-error ip "IPv6 must consist of 8 or less parts."))
    (loop for part in parts
          for num = (ignore-errors (parse-integer part :radix 16))
          for i from 0
          unless (or (not num) (<= #x0000 num #xFFFF))
            do (ratification-error ip "~s is not a hexadecimal integer between 0 and FFFF." part))))

(define-test ip (ip)
  (cond ((find #\: ip)
         (test-ipv6 ip))
        ((find #\. ip)
         (test-ipv4 ip))
        (T (ratification-error ip "This is neither an IPv4 nor an IPv6."))))

(define-test host (host)
  (when (and (char= (aref host 0) #\[)
             (char= (aref host (1- (length host))) #\]))
    (test-ip (subseq host 1 (1- (length host)))))
  (loop for char across host
        unless (or (unreserved-character-p char)
                   (percent-encoded-p char)
                   (sub-delimiter-p char))
          do (ratification-error host "Invalid character ~a. Host can only contain alphanumerics or - . _ ~~ % ! $ & ' ( ) * + , ; =")))

(define-test scheme (scheme)
  (unless (< 0 (length scheme))
    (ratification-error scheme "A scheme must be at least one character long."))
  (unless (true-alpha-p (aref scheme 0))
    (ratification-error scheme "Scheme must start with an alphabetic character."))
  (loop for i from 1 below (length scheme)
        for char = (aref scheme i)
        unless (or (true-alphanumeric-p char)
                   (find char "-.+" :test #'char=))
          do (ratification-error scheme "Invalid character ~a. Scheme can only contain alphanumerics or - . +" char)))

(define-test user (user)
  (loop for char in user
        unless (or (unreserved-character-p char)
                   (sub-delimiter-p char)
                   (percent-encoded-p char)
                   (char= char #\:))
          do (ratification-error user "Invalid character ~a. Username can only contain alphanumerics or % ! $ & ' ( ) * + , ; = - . _ ~~ :")))

(define-test port (port)
  (let ((num (ignore-errors (parse-integer port))))
    (unless num
      (ratification-error port "Port must be a decimal integer."))
    (unless (<= 0 port 65535)
      (ratification-error port "Port must be between 0 and 65535."))))

(define-test authority (authority)
  (let ((atpos (position #\@ authority)))
    (when atpos
      (test-user (subseq authority 0 atpos))
      (setf authority (subseq authority (1+ atpos)))))
  (let ((colonpos (position #\: authority)))
    (when colonpos
      (test-port (subseq authority (1+ colonpos)))
      (setf authority (subseq authority 0 colonpos))))
  (test-host authority))

(define-test rootless-path (path)
  (loop for char in path
        unless (pchar-p char)
          do (ratification-error path "Invalid character ~a. Path can only contain alphanumerics or ! $ & ' ( ) * + , ; = - . _ ~~ : @")))

(define-test absolute-path (path)
  (unless (< 0 (length path))
    (ratification-error path "Path must be at least one character long."))
  (unless (char= (aref path 0) #\/)
    (ratification-error path "An absolute path must start with a forward slash."))
  (when (< 1 (length path))
    (when (char= (aref path 1) #\/)
      (ratification-error path "Beginning slash must be followed by a non-slash character."))
    (test-rootless-path (subseq path 1))))

(define-test hierarchical-part (hierarchical)
  (let ((length (length hierarchical)))
    (when (and (= length 1) (string/= hierarchical "/"))
      (ratification-error hierarchical "Hierarchical part must be either a path or begin with //."))
    (when (and (< 1 (length hierarchical))
               (string= "//" hierarchical :end2 2))
      (let ((slashpos (position #\/ hierarchical)))
        (if slashpos
            (progn (test-authority (subseq hierarchical 0 slashpos))
                   (test-absolute-path (subseq hierarchical slashpos)))
            (test-authority hierarchical))))
    (test-absolute-path hierarchical)))

(define-test query (query)
  (loop for char in query
        unless (or (pchar-p char)
                   (find char "?/" :test #'char=))
          do (ratification-error query "Invalid character ~a. Query can only contain alphanumercs or ! $ & ' ( ) * + , ; = - . _ ~~ : @ ? /")))

(define-test fragment (fragment)
  (loop for char in fragment
        unless (or (pchar-p char)
                   (find char "?/" :test #'char=))
          do (ratification-error fragment "Invalid character ~a. Fragment can only contain alphanumercs or ! $ & ' ( ) * + , ; = - . _ ~~ : @ ? /")))

(define-test uri (uri)
  (or
   (cl-ppcre:register-groups-bind (scheme hierarchical NIL query NIL fragment) ("^([^:]+):([^\\?]+)(\\?([^#]*))?(\\#(.*))?$" uri)
     (when scheme (test-scheme scheme))
     (when hierarchical (test-hierarchical-part hierarchical))
     (when query (test-query query))
     (when fragment (test-fragment fragment)))
   (ratification-error uri "Uri must consist of at least a scheme followed by a colon and a path.")))
