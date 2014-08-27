#|
 This file is a part of ratify
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.ratify.asdf
  (:use #:cl #:asdf))
(in-package #:org.tymoonnext.ratify.asdf)

(defsystem ratify
  :name "Ratify"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A collection of utilities to ratify and validate inputs."
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "testing")
               (:file "uri")
               (:file "url")
               (:file "email")
               (:file "date")
               (:file "types"))
  :depends-on (:cl-ppcre))
