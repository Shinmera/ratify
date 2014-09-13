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
  :version "0.1.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A collection of utilities to ratify, validate and parse inputs."
  :homepage "https://github.com/Shinmera/ratify"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "testing")
               (:file "parsing")
               (:file "uri")
               (:file "url")
               (:file "email")
               (:file "css")
               (:file "date")
               (:file "types")
               (:file "html"))
  :depends-on (:cl-ppcre
               :local-time
               :parse-float))
