(asdf:defsystem supervisor.test
  :version "0"
  :description "Grouping of threads to die or run together"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "LLGPL"
  :depends-on (supervisor lisp-unit)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "supervisor-test")))
