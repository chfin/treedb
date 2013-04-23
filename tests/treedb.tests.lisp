;;;; treedb.tests.lisp

(in-package #:treedb.tests)

(def-suite treedb-all
    :description "Test suite containing all tests for treedb")

(def-suite treedb-alist :in treedb-all
	   :description "Tests for alist implementation of treedb")

(defun test-basics (db)
  (is (= 1 (setf (node db :a) 1)))
  (is (= 1 (node db :a)))
  (is (= 2 (setf (node db :b :1) 2)))
  (is (= 2 (node db :b :1)))
  (is (= 3 (setf (node db :b :1) 3)))
  (is (eq nil (node db :b :2)))
  (is (eq nil (node db :b :1 :x)))
  (is (= 3 (node db :b :1))))

(defun test-advanced (db)
  (setf (node db :a :1) 1)
  (signals node-no-leaf-error
    (node db :a) 2)
  (signals node-no-leaf-error
    (setf (node db :a) 2)))

(defun test-delete (db)
  (setf (node db :a :1) 1)
  (setf (node db :a :2) 2)
  (is (= (del-node db :a :1) 1))
  (is (not (node-p db :a :1)))
  (is (= (node db :a :2) 2))
  (is (node-p db :a))
  (is (null (del-node db :a)))
  (is (not (node-p db :a)))
  (signals node-not-found-error (del-node db :b)))

(defun test-predicates (db)
  (setf (node db :a :1) 1)
  (is (node-p db :a :1))
  (is (leaf-p db :a :1))
  (is (node-p db :a))
  (is (not (leaf-p db :a)))
  (is (not (node-p db :b)))
  (is (not (leaf-p db :b)))
  (is (not (node-p db :a :1 :x)))
  (is (not (leaf-p db :a :1 :x))))

(defun test-structure (db)
  (setf (node db :a :1) 1
	(node db :a :2) 2
	(node db :a :3 :x) 0
	(node db :a :3 :y) -1)
  (is (equal '(:a) (children db)))
  (is (member :1 (children db :a)))
  (is (member :2 (children db :a)))
  (is (member :3 (children db :a)))
  (is (member :x (children db :a :3)))
  (is (member :y (children db :a :3)))
  (let ((sub (subtree db :a)))
    (is (= (node sub :1) 1) "not 1")
    (is (= (node sub :2) 2) "not 2")
    (is (= (node sub :3 :x) 0) "not 0")
    (is (= (node sub :3 :y) -1) "not -1")))

(defun test-conversions (db)
  (setf (node db :a) 1
	(node db :b :1) 2
	(node db :b :2) 3)
  (let ((a (to-alist db))
	(j (to-json db)))
    (is (= (cdr (assoc :a a)) 1))
    (is (= (cdr (assoc :1 (cdr (assoc :b a)))) 2))
    (is (= (cdr (assoc :2 (cdr (assoc :b a)))) 3))
    (is (typep j 'string))))

(in-suite treedb-alist)

(test alist-basics
  "Basic tests of the core functionality of alist"
  (test-basics (make-alist-treedb)))

(test alist-advanced
  "Advanced tests for alist"
  (test-advanced (make-alist-treedb)))

(test alist-delete
  "Tests for del-node on alist"
  (test-delete (make-alist-treedb)))

(test alist-predicates
  "Tests for predicate methods on alist"
  (test-predicates (make-alist-treedb)))

(test alist-structure
  "Tests for subtree and children on alist"
  (test-structure (make-alist-treedb)))

(test alist-conversions
  "Tests for conversions on alist"
  (test-conversions (make-alist-treedb)))
