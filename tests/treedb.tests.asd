;;;; treedb.tests.asd

(asdf:defsystem #:treedb.tests
  :serial t
  :description "Tests for treedb"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT"
  :depends-on (#:treedb
               #:fiveam)
  :components ((:file "package")
               (:file "treedb.tests")))

