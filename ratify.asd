(defsystem ratify
  :name "Ratify"
  :version "0.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A collection of utilities to ratify, validate and parse inputs."
  :homepage "https://shinmera.com/docs/ratify/"
  :bug-tracker "https://shinmera.com/project/ratify/issues"
  :source-control (:git "https://shinmera.com/project/ratify.git")
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
