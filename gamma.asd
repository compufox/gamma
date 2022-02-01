;;;; gamma.asd

(asdf:defsystem #:gamma
  :description "Describe gamma here"
  :author "ava fox <dev@computerfox.xyz>"
  :license  "NPLv1+"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-template #:unix-opts #:cl-markdown #:simple-config)
  :components ((:file "package")
               (:file "gamma"))
  
  :build-operation "program-op"
  :build-pathname "bin/gamma"
  :entry-point "gamma:main")
