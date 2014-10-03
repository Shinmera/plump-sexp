#|
 This file is a part of Plump
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.plump.sexp.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.plump.sexp.asdf)

(defsystem plump-sexp
  :name "Plump-SEXP"
  :version "0.1.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Turning SEXPs into a Plump DOM and back."
  :homepage "https://github.com/Shinmera/plump-sexp"
  :serial T
  :components ((:file "plump-sexp"))
  :depends-on (:plump))
