;;;; treedb.doc.lisp

(in-package #:treedb.doc)

(defun create-doc ()
  "Creates the documentation (doc/treedb.html) using cl-gendoc."
  (gendoc:gendoc (:output-filename "treedb.html" :css "ghs-doc.css" :title "treedb - a hierarchical key-value-store" :output-system :treedb.doc)
    (:mdf "js.md")
    (:mdf "../README.md")
    (:apiref #:treedb #:treedb.tests #:treedb.doc)))
