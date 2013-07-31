(asdf:defsystem thread-group.test
  :version "0"
  :description "Grouping of threads to die or run together"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "LLGPL"
  :depends-on (thread-group lisp-unit)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "thread-group-test")))
