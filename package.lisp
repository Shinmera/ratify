#|
 This file is a part of ratify
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(defpackage #:ratify-toolkit
  (:nicknames #:org.tymoonnext.ratify.toolkit)
  (:use #:cl)
  (:export
   #:ratification-error
   #:message
   #:test-object
   #:ratification-error
   #:make-keyword
   #:true-alpha-p
   #:true-alphanumeric-p))

(defpackage #:ratify-testing
  (:nicknames #:org.tymoonnext.ratify.testing)
  (:use #:cl #:ratify-toolkit)
  (:export
   #:test-failed
   #:test-name
   #:test-object
   #:cause
   #:combined-error
   #:errors
   #:test
   #:define-test
   #:with-skipping
   #:skip-error
   #:skippable-error
   #:with-errors-combined
   #:perform-test
   #:perform-tests
   #:perform-combined-tests))

(defpackage #:ratify-parsing
  (:nicknames #:org.tymoonnext.ratify.parsing)
  (:use #:cl #:ratify-testing #:ratify-toolkit)
  (:export
   #:parser
   #:define-parser
   #:parse
   #:with-parsed-forms))

(defpackage #:ratify-uri
  (:nicknames #:org.tymoonnext.ratify.uri)
  (:use #:cl #:ratify-toolkit #:ratify-testing)
  (:export
   #:test-ipv4
   #:test-ipv6
   #:test-ip
   #:test-host
   #:test-scheme
   #:test-user
   #:test-port
   #:test-authority
   #:test-path-segment
   #:test-rootless-path
   #:test-absolute-path
   #:test-hierarchical-part
   #:test-query
   #:test-fragment
   #:test-uri
   #:ipv4-p
   #:ipv6-p
   #:ip-p
   #:host-p
   #:scheme-p
   #:user-p
   #:port-p
   #:authority-p
   #:path-segment-p
   #:rootless-path-p
   #:absolute-path-p
   #:hierarchical-part-p
   #:query-p
   #:fragment-p
   #:uri-p))

(defpackage #:ratify-url
  (:nicknames #:org.tymoonnext.ratify.url)
  (:use #:cl #:ratify-toolkit #:ratify-testing #:ratify-uri)
  (:export
   #:*permitted-protocols*
   #:test-hostname
   #:test-domain
   #:test-protocol
   #:test-url
   #:hostname-p
   #:domain-p
   #:protocol-p
   #:url-p))

(defpackage #:ratify-email
  (:nicknames #:org.tymoonnext.ratify.email)
  (:use #:cl #:ratify-toolkit #:ratify-testing #:ratify-url)
  (:export
   #:test-email
   #:email-p))

(defpackage #:ratify-css
  (:nicknames #:org.tymoonnext.ratify.css)
  (:use #:cl #:ratify-toolkit #:ratify-testing)
  (:export
   #:test-color
   #:test-property
   #:color-p
   #:property-p))

(defpackage #:ratify-date
  (:nicknames #:org.tymoonnext.ratify.date #:org.tymoonnext.ratify.time #:ratify-time)
  (:use #:cl #:ratify-toolkit #:ratify-testing #:ratify-parsing)
  (:export
   #:test-year
   #:test-month
   #:test-day
   #:test-hour
   #:test-minute
   #:test-second
   #:test-offset
   #:test-time
   #:test-date
   #:test-datetime
   #:parse-year
   #:parse-month
   #:parse-day
   #:parse-hour
   #:parse-minute
   #:parse-second
   #:parse-offset
   #:parse-time
   #:parse-date
   #:parse-datetime
   #:year-p
   #:month-p
   #:day-p
   #:hour-p
   #:minute-p
   #:second-p
   #:offset-p
   #:time-p
   #:date-p
   #:datetime-p))

(defpackage #:ratify-types
  (:nicknames #:org.tymoonnext.ratify.types)
  (:use #:cl #:ratify-toolkit #:ratify-testing #:ratify-parsing)
  (:export
   #:test-bit
   #:test-unsigned-integer
   #:test-integer
   #:test-ratio
   #:test-rational
   #:test-float
   #:test-real
   #:test-complex
   #:test-number
   #:test-boolean
   #:test-character
   #:test-string
   #:test-alphabetic
   #:test-numeric
   #:test-alphanumeric
   #:parse-bit
   #:parse-unsigned-integer
   #:parse-integer
   #:parse-ratio
   #:parse-rational
   #:parse-float
   #:parse-real
   #:parse-complex
   #:parse-number
   #:parse-boolean
   #:parse-character
   #:parse-string
   #:bit-p
   #:unsigned-integer-p
   #:integer-p
   #:ratio-p
   #:rational-p
   #:float-p
   #:real-p
   #:complex-p
   #:number-p
   #:boolean-p
   #:character-p
   #:string-p
   #:alphabetic-p
   #:numeric-p
   #:alphanumeric-p))

(defpackage #:ratify-html
  (:nicknames #:org.tymoonnext.ratify.html)
  (:use #:cl #:ratify-toolkit #:ratify-testing #:ratify-url #:ratify-email #:ratify-date #:ratify-css #:ratify-types)
  (:export
   #:test-checkbox
   #:test-color
   #:test-date
   #:test-datetime
   #:test-datetime-local
   #:test-email
   #:test-file
   #:test-month
   #:test-number
   #:test-password
   #:test-radio
   #:test-range
   #:test-search
   #:test-tel
   #:test-text
   #:test-textarea
   #:test-time
   #:test-url
   #:test-week
   #:checkbox-p
   #:color-p
   #:date-p
   #:datetime-p
   #:datetime-local-p
   #:email-p
   #:file-p
   #:month-p
   #:number-p
   #:password-p
   #:radio-p
   #:range-p
   #:search-p
   #:tel-p
   #:text-p
   #:textarea-p
   #:time-p
   #:url-p
   #:week-p))

(defpackage #:ratify
  (:nicknames #:org.tymoonnext.ratify)
  (:use #:cl))

(let ((ratify (find-package '#:ratify)))
  (dolist (pkg '(#:ratify-toolkit #:ratify-testing #:ratify-parsing #:ratify-email #:ratify-css #:ratify-uri #:ratify-url #:ratify-date #:ratify-types #:ratify-html))
    (do-external-symbols (symb (find-package pkg))
      (import symb ratify)
      (export symb ratify))))
