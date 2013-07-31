(asdf:defsystem thread-group
  :version "0"
  :description "Grouping of threads to die or run together"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :licence "LLGPL"
  :depends-on (bordeaux-threads)
  :serial t
  ;; components likely need manual reordering
  :components ((:file "thread-group")))
