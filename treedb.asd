;;;; treedb.asd

(asdf:defsystem #:treedb
  :serial t
  :depends-on (#:cl-json)
  :description "A hierarchical key-value-database"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT"
  :components ((:file "package")
               (:file "treedb")))
