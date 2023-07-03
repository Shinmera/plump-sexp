(defsystem plump-sexp
  :name "Plump-SEXP"
  :version "0.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Turning SEXPs into a Plump DOM and back."
  :homepage "https://Shinmera.github.io/plump-sexp/"
  :bug-tracker "https://github.com/Shinmera/plump-sexp/issues"
  :source-control (:git "https://github.com/Shinmera/plump-sexp.git")
  :serial T
  :components ((:file "plump-sexp"))
  :depends-on (:plump))
