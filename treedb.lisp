;;;; treedb.lisp

(in-package #:treedb)

;;; api definition

(defgeneric node (db &rest keys)
  (:documentation "=> (the value at `/k/e/y/s`, `t` / `nil`)
Returns the value of a leaf node as the first
and `t` or `nil` as the second value,
depending on if the node was found or not.
Can be used with `setf`."))

(defgeneric (setf node) (val db &rest keys)
  (:documentation "=> `val`
Sets the value at `/k/e/y/s` to `val`."))

(defgeneric del-node (db &rest keys)
  (:documentation "=> the deleted value
Deletes the value at `/k/e/y/s`."))

(defgeneric node-p (db &rest keys)
  (:documentation
   "=> `t` or `nil`
Tests, if `/k/e/y/s` is a node in `db`."))

(defgeneric leaf-p (db &rest keys)
  (:documentation
   "=> `t` or `nil`
Tests, if `/k/e/y/s` is a leaf in `db`."))

(defgeneric subtree (db &rest keys)
  (:documentation "=> subtree at `/k/e/y/s`
Returns a new tree object with the contents of the subtree.
The new object has the same type as `db`."))

(defgeneric children (db &rest keys)
  (:documentation "=> children of node `/k/e/y/s`
Returns a list of keys, which are the children of the given node."))

(defgeneric to-alist (db)
  (:documentation "=> a nested alist
Returns the tree in alist form"))

(defgeneric to-json (db &optional stream)
  (:documentation "=> a json string or `nil`
Serializes the tree into json objects.
If `stream` is supplied, the json object is written to the corresponding stream.
Otherwise it is returned as a string."))

(define-condition node-no-leaf-error (error) ()
  (:documentation "Signaled, if a given node is not a leaf."))

(define-condition node-not-found-error (error) ()
  (:documentation "Signaled, if a node to delete was not found in the tree."))

(define-condition malformed-tree-error (error) ()
  (:documentation "Signaled, if an internal error occurs. Probably means, you found a bug."))

;;; utils

(defmacro node-or (db default &rest keys)
  "=> the value at `/k/e/y/s or `default`
Returns the value at `\k\e\y\s or, if it is not found, `default`."
  (let ((res (gensym "result"))
	(success (gensym "success")))
    `(multiple-value-bind (,res ,success) (node ,db ,@keys)
       (if ,success ,res ,default))))

;;; helpers

(defun to-key (src)
  (intern (format nil "~a" src) :keyword))

(defun to-keys (keys)
  (mapcar #'to-key keys))

(defun subtree-p (sub)
  (and (consp sub) (eq (car sub) :tree)))

(defun value-p (sub)
  (and (consp sub) (eq (car sub) :value)))

;;; alist implementation

(defclass <alist-treedb> ()
  ((root :accessor root
	 :initarg :root
	 :initform (list :tree)
	 :type list)))

(defun make-alist-treedb ()
  "Returns an empty tree of type `<alist-treedb>`."
  (make-instance '<alist-treedb>))

(defmethod node ((db <alist-treedb>) &rest keys)
  (labels ((val (sub keys)
	     (if (and keys (subtree-p sub))
		 (val (cdr (assoc (car keys) (cdr sub))) (cdr keys))
		 (if keys
		     (values nil nil)
		     (if (and (consp sub) (eq :tree (car sub)))
			 (error 'node-no-leaf-error)
			 (values (cdr sub) t))))))
    (val (root db) (to-keys keys))))

(defmethod (setf node) (val (db <alist-treedb>) &rest keys)
  (labels ((make-sub (keys val) ;creates a new subtree for the remaining keys
	     (if keys
		 (cons :tree (list (cons (car keys) (make-sub (cdr keys) val))))
		 (cons :value val)))
	   (setn (sub keys val) ;searches the tree and sets the value
	     (if (subtree-p sub) ;4 cases:
		 ;sub is a subtree
		 (if keys
		     ;1
		     (cons :tree
			   (cons (if (assoc (car keys) (cdr sub)) ;key in list?
				     (cons (car keys)
					   (setn (cdr (assoc (car keys)
							     (cdr sub)))
						 (cdr keys) val))
				     (cons (car keys) (make-sub (cdr keys) val)))
				 (remove (car keys) (cdr sub) :key #'car)))
		     ;2
		     (error 'node-no-leaf-error))
		 ;sub is a value
		 (if keys
		     ;3
		     (make-sub keys val)
		     ;4
		     (cons :value val)))))
    (with-slots (root) db
      (setf root (setn root (to-keys keys) val)))
    val))

(defmethod del-node ((db <alist-treedb>) &rest keys)
  (let ((val nil))
    (labels ((del (sub keys)
	       (if keys
		   (let ((entry (cdr (assoc (car keys) (cdr sub)))))
		     (if entry
			 (let ((newsub (del entry (cdr keys)))
			       (stripped (remove (car keys)
						 (cdr sub) :key #'car)))
			   (cons :tree
				 (if newsub
				     (cons (cons (car keys) newsub) stripped)
				     stripped)))
			 (error 'node-not-found-error)))
		   (when (value-p sub)
		     (setf val (cdr sub))
		     nil))))
      (with-slots (root) db
	(setf root (del root (to-keys keys))))
      val)))

(defmethod node-p ((db <alist-treedb>) &rest keys)
  (labels ((ex (sub keys)
	     (if (subtree-p sub)
		 (if keys
		     (let ((entry (assoc (car keys) (cdr sub))))
		       (when entry
			 (ex (cdr entry) (cdr keys))))
		     t)
		 (not keys))))
    (ex (root db) (to-keys keys))))

(defmethod leaf-p ((db <alist-treedb>) &rest keys)
  (labels ((ex (sub keys)
	     (if (subtree-p sub)
		 (when keys
		   (let ((entry (assoc (car keys) (cdr sub))))
		     (when entry
		       (ex (cdr entry) (cdr keys)))))
		 (not keys))))
    (ex (root db) (to-keys keys))))

(defmethod subtree ((db <alist-treedb>) &rest keys)
  (labels ((subtr (sub keys)
	     (if keys
		 (when (subtree-p sub)
		   (let ((entry (cdr (assoc (car keys) (cdr sub)))))
		     (when entry
		       (subtr entry (cdr keys)))))
		 sub)))
    (let ((sub (subtr (root db) keys)))
      (when sub (make-instance '<alist-treedb> :root sub)))))

(defmethod children ((db <alist-treedb>) &rest keys)
  (labels ((chld (sub keys)
	     (if keys
		 (if (and (subtree-p sub) (assoc (car keys) (cdr sub)))
		     (chld (cdr (assoc (car keys) (cdr sub))) (cdr keys))
		     (error 'note-not-found-error))
		 (when (subtree-p sub)
		   (mapcar #'car (cdr sub))))))
    (chld (root db) (to-keys keys))))

(defmethod to-alist ((db <alist-treedb>))
  (labels ((alst (sub)
	     (cond
	       ((subtree-p sub) (mapcar (lambda (e)
					  (cons (car e) (alst (cdr e))))
					(cdr sub)))
	       ((value-p sub) (cdr sub))
	       (t (error 'malformed-tree-error)))))
    (alst (root db))))

(defmethod to-json ((db <alist-treedb>) &optional stream)
  (if stream
      (cl-json:encode-json-alist (to-alist db) stream)
      (cl-json:encode-json-alist-to-string (to-alist db))))
