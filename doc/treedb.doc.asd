;;;; treedb.doc.asd

(asdf:defsystem #:treedb.doc
  :serial t
  :description "Used to create documentation for treedb"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT"
  :depends-on (#:treedb
	       #:treedb.tests
               #:cl-gendoc)
  :components ((:file "package")
               (:file "treedb.doc")
	       (:static-file "ghs-doc.css")))
