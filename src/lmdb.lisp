(in-package :cl-user)
(defpackage lmdb
  (:use :cl)
  (:shadow :get)
  ;; Classes
  (:export :environment
           :make-environment
           :transaction
           :transaction-environment
           :transaction-parent
           :make-transaction
           :database
           :make-database
           :cursor
           :make-cursor)
  ;; Lifecycle
  (:export :open-environment
           :close-environment
           :begin-transaction
           :commit-transaction
           :abort-transaction
           :open-database
           :close-database
           :open-cursor
           :close-cursor)
  ;; Macros
  (:export :with-environment
           :with-database
           :with-cursor
           :do-pairs)
  ;; Errors
  (:export :lmdb-error)
  ;; Methods
  (:export :version-string
           :environment-statistics
           :environment-info
           :get
           :put
           :del
           :cursor-get)
  (:documentation "The high-level LMDB interface."))
(in-package :lmdb)

;;; Library

(cffi:define-foreign-library liblmdb
  (:darwin (:or "liblmdb.dylib" "liblmdb.1.dylib"))
  (:unix  (:or "liblmdb.so" "liblmdb.so.0" "liblmdb.so.0.0.0"))
  (:win32 "liblmdb.dll")
  (t (:default "liblmdb")))

(cffi:use-foreign-library liblmdb)

;;; Constants

(defparameter +permissions+ #o664
  "The Unix permissions to use for the database.")

;; Some error codes
(defparameter +enoent+ 2)
(defparameter +eacces+ 13)
(defparameter +eagain+ 11)

;;; Classes

(defclass environment ()
  ((handle :reader %handle
           :initarg :handle
           :documentation "The pointer to the environment handle.")
   (directory :reader environment-directory
              :initarg :directory
              :documentation "The directory where environment files are stored.")
   (max-databases :reader environment-max-dbs
                  :initarg :max-dbs
                  :initform 1
                  :type integer
                  :documentation "The maximum number of named databases."))
  (:documentation "Environment handle."))

(defclass transaction ()
  ((handle :reader %handle
           :initarg :handle
           :documentation "The pointer to the transaction.")
   (env :reader transaction-environment
        :initarg :environment
        :type environment
        :documentation "The environment this transaction belongs to.")
   (parent :reader transaction-parent
           :initarg :parent
           :type (or transaction null)
           :documentation "The parent transaction, if any."))
  (:documentation "A transaction."))

(defclass database ()
  ((handle :reader %handle
           :initarg :handle
           :documentation "The DBI handle.")
   (transaction :reader database-transaction
                :initarg :transaction
                :type transaction
                :documentation "The transaction this database belongs to.")
   (name :reader database-name
         :initarg :name
         :type string
         :documentation "The database name.")
   (create :reader database-create-p
           :initarg :create
           :type boolean
           :documentation "Whether or not to create the database if it doesn't
           exist."))
  (:documentation "A database."))

(defstruct (value (:constructor %make-value))
  "A value is a generic representation of keys and values."
  (size 0 :type fixnum)
  data)

(defclass cursor ()
  ((handle :reader %handle
           :initarg :handle
           :documentation "The pointer to the cursor object.")
   (database :reader cursor-database
             :initarg :database
             :type database
             :documentation "The database the cursor belongs to."))
  (:documentation "A cursor."))

;;; Constructors

(defun make-environment (directory &key (max-databases 1) (mapsize (* 10 1024 1024)))
  "Create an environment object.

Before an environment can be used, it must be opened with @c(open-environment)."
  (let ((instance (make-instance 'environment
                                 :handle (cffi:foreign-alloc :pointer)
                                 :directory directory
                                 :max-dbs max-databases)))
    (unless (= (liblmdb:env-create (%handle instance))
               0)
      (error "Error creating environment object."))
    ;; Set the maximum number of databases
    (liblmdb:env-set-maxdbs (handle instance) (environment-max-dbs instance))
    ;; Set memory map size
    (liblmdb:env-set-mapsize (handle instance) mapsize)
    instance))

(defun make-transaction (environment &optional parent)
  "Create a transaction object."
  (make-instance 'transaction
                 :handle (cffi:foreign-alloc :pointer)
                 :environment environment
                 :parent parent))

(defun make-database (transaction name
                      &key (create t))
  "Create a database object."
  (make-instance 'database
                 :handle (cffi:foreign-alloc :pointer)
                 :transaction transaction
                 :name name
                 :create create))

(defun convert-data (data)
  "Convert Lisp data to a format LMDB likes. Supported types are integers,
floats, booleans and strings. Returns a (size . array) pair."
  (typecase data
    (string
     (let ((octets (trivial-utf-8:string-to-utf-8-bytes data)))
       (cons (length octets) octets)))
    (vector
     (cons (length data) data))
    (integer
     (let ((octets (integer-to-octets data)))
       (cons (length octets) octets)))
    (boolean
     (cons 1 (if data 1 0)))
    (t
     (error "Invalid type."))))



(defun make-value (data)
  "Create a value object."
  (destructuring-bind (size . vector)
      (convert-data data)
    (%make-value :size size
                 :data vector)))

(defun make-cursor (database)
  "Create a cursor object."
  (make-instance 'cursor
                 :handle (cffi:foreign-alloc :pointer)
                 :database database))

;;; Errors

(define-condition lmdb-error ()
  ()
  (:documentation "The base class of all LMDB errors."))

(defun unknown-error (error-code)
  (error "Unknown error code: ~A. Result from strerror(): ~A"
         error-code
         (liblmdb:strerror error-code)))

;;; Viscera

(defun handle (object)
  "Return the handle from an environment, transaction or database."
  (cffi:mem-ref (%handle object) :pointer))

(defun open-environment (environment)
  "Open the environment connection.

@begin(deflist)
@term(Thread Safety)

@def(No special considerations.)

@end(deflist)"
  (with-slots (directory) environment
    (assert (uiop:directory-pathname-p directory))
    (ensure-directories-exist directory)
    (let ((return-code (liblmdb:env-open (handle environment)
                                          (namestring directory)
                                          0
                                          (cffi:make-pointer +permissions+))))
      (alexandria:switch (return-code)
        (liblmdb:+version-mismatch+
         (error "Version mismatch: the client version is different from the environment version."))
        (liblmdb:+invalid+
         (error "Data corruption: the environment header files are corrupted."))
        (+enoent+
         (error "The environment directory doesn't exist."))
        (+eacces+
         (error "The user doesn't have permission to use the environment directory."))
        (+eagain+
         (error "The environment files are locked by another process."))
        (0
         ;; Success
         )
        (t
         (unknown-error return-code)))))
  environment)

(defun environment-statistics (environment)
  "Return statistics about the environment."
  (cffi:with-foreign-object (stat '(:struct liblmdb:stat))
    (liblmdb:env-stat (handle environment)
                       stat)
    (macrolet ((slot (slot)
                 `(cffi:foreign-slot-value stat
                                           '(:struct liblmdb:stat)
                                           ',slot)))
      (list :page-size (slot liblmdb:ms-psize)))))

(defun environment-info (environment)
  "Return information about the environment."
  (cffi:with-foreign-object (info '(:struct liblmdb:envinfo))
    (liblmdb:env-info (handle environment)
                       info)
    (macrolet ((slot (slot)
                 `(cffi:foreign-slot-value info
                                           '(:struct liblmdb:envinfo)
                                           ',slot)))
      (list :map-address (cffi:pointer-address (slot liblmdb:me-mapaddr))
            :map-size (cffi:pointer-address (slot liblmdb:me-mapsize))))))

(defun begin-transaction (transaction)
  "Begin the transaction.

@begin(deflist)
@term(Thread Safety)

@def(A transaction may only be used by a single thread.)

@end(deflist)"
  (with-slots (env parent) transaction
    (let ((return-code (liblmdb:txn-begin (handle env)
                                           (if parent
                                               (handle parent)
                                               (cffi:null-pointer))
                                           0
                                           (%handle transaction))))
      (alexandria:switch (return-code)
        (0
         ;; Success
         t)
        ;; TODO rest
        (t
         (unknown-error return-code))))))

(defun commit-transaction (transaction)
  "Commit the transaction. The transaction pointer is freed.

@begin(deflist)
@term(Thread Safety)

@def(The LMDB documentation doesn't say specifically, but assume it can only be
called by the transaction-creating thread.)

@end(deflist)"
  (let ((return-code (liblmdb:txn-commit (handle transaction))))
    (alexandria:switch (return-code :test #'=)
      (0
       ;; Success
       t)
      (t
       (unknown-error return-code)))))

(defun abort-transaction (transaction)
  "Abort the transaction. The transaction pointer is freed.

@begin(deflist)
@term(Thread Safety)

@def(The LMDB documentation doesn't say specifically, but assume it can only be
called by the transaction-creating thread.)

@end(deflist)"
  (liblmdb:txn-abort (handle transaction)))

(defun open-database (database)
  "Open a database.

@begin(deflist)
@term(Thread Safety)

@def(A transaction that opens a database must finish (either commit or abort)
before another transaction may open it. Multiple concurrent transactions cannot
open the same database.)

@end(deflist)"
  (with-slots (transaction name create) database
    (let ((return-code (liblmdb:dbi-open (handle transaction)
                                          name
                                          (logior 0
                                                  (if create
                                                      liblmdb:+create+
                                                      0))
                                          (%handle database))))
      (alexandria:switch (return-code)
        (0
         ;; Success
         t)
        (liblmdb:+notfound+
         (error "Database not found, and did not specify :create t."))
        (liblmdb:+dbs-full+
         (error "Reached maximum number of named databases."))
        (t
         (unknown-error return-code)))))
  database)

(defun open-cursor (cursor)
  "Open a cursor."
  (with-slots (database) cursor
    (with-slots (transaction) database
      (let ((return-code (liblmdb:cursor-open (handle transaction)
                                               (handle database)
                                               (%handle cursor))))
        (alexandria:switch (return-code)
          (0
           ;; Success
           t)
          (22
           (error "Invalid parameter."))
          (t
           (unknown-error return-code))))))
  cursor)

;;; Querying

(defmacro with-val ((raw-value data) &body body)
  (alexandria:with-gensyms (value-struct array)
    `(let* ((,value-struct (make-value ,data))
            (,raw-value (cffi:foreign-alloc '(:struct liblmdb:val)))
            (,array (cffi:foreign-alloc :unsigned-char
                                        :count (value-size ,value-struct))))
       (setf (cffi:foreign-slot-value ,raw-value
                                      '(:struct liblmdb:val)
                                      'liblmdb:mv-size)
             (cffi:make-pointer (value-size ,value-struct)))

       (loop for elem across (value-data ,value-struct)
             for i from 0 to (1- (length (value-data ,value-struct)))
             do
                (setf (cffi:mem-aref ,array :unsigned-char i)
                      elem))
       (setf (cffi:foreign-slot-value ,raw-value
                                      '(:struct liblmdb:val)
                                      'liblmdb:mv-data)
             ,array)
       ,@body)))

(defmacro with-empty-value ((value) &body body)
  `(cffi:with-foreign-object (,value '(:struct liblmdb:val))
     ,@body))

(defun raw-value-to-vector (raw-value)
  (let* ((size (cffi:pointer-address
                (cffi:foreign-slot-value raw-value
                                         '(:struct liblmdb:val)
                                         'liblmdb:mv-size)))
         (array (cffi:foreign-slot-value raw-value
                                         '(:struct liblmdb:val)
                                         'liblmdb:mv-data))
         (vec (make-array size
                          :element-type '(unsigned-byte 8))))
    (loop for i from 0 to (1- size) do
      (setf (elt vec i)
            (cffi:mem-aref array :unsigned-char i)))
    vec))

(defun get (database key)
  "Get a value from the database."
  (with-slots (transaction) database
    (with-val (raw-key key)
      (with-empty-value (raw-value)
        (let ((return-code (liblmdb:get (handle transaction)
                                         (handle database)
                                         raw-key
                                         raw-value)))
          (alexandria:switch (return-code)
            (0
             ;; Success
             (values (raw-value-to-vector raw-value) t))
            (liblmdb:+notfound+
             (values nil nil))
            (t
             (unknown-error return-code))))))))

(defun put (database key value)
  "Add a value to the database."
  (with-slots (transaction) database
    (with-val (raw-key key)
      (with-val (raw-val value)
        (let ((return-code (liblmdb:put (handle transaction)
                                         (handle database)
                                         raw-key
                                         raw-val
                                         0)))
          (alexandria:switch (return-code)
            (0
             ;; Success
             t)
            (t
             (unknown-error return-code)))))))
  value)

(defun del (database key &optional data)
  "Delete this key from the database. Returns @c(t) if the key was found,
@c(nil) otherwise."
  (with-slots (transaction) database
    (with-val (raw-key key)
      (let ((return-code (liblmdb:del (handle transaction)
                                       (handle database)
                                       raw-key
                                       (if data
                                           data
                                           (cffi:null-pointer)))))
        (alexandria:switch (return-code)
          (0
           ;; Success
           t)
          (liblmdb:+notfound+
           nil)
          (+eacces+
           (error "An attempt was made to delete a key in a read-only transaction."))
          (t
           (unknown-error return-code)))))))

(defun cursor-get (cursor operation &optional key)
  "Extract data using a cursor.

The @cl:param(operation) argument specifies the operation."
  (let ((op (case operation
              (:first :+first+)
              (:current :+get-current+)
              (:last :+last+)
              (:next :+next+)
              (:prev :+prev+)
              ;; These three require a KEY argument
              (:set :+set+)
              (:set-key :+set-key+)
              (:set-range :+set-range+)
              )))
    (flet ((body (raw-key raw-value)
             (let ((return-code (liblmdb:cursor-get (handle cursor)
                                                    raw-key
                                                    raw-value
                                                    op)))
               (alexandria:switch (return-code)
                 (0
                  ;; Success
                  (values (raw-value-to-vector raw-key)
                          (raw-value-to-vector raw-value)))
                 (liblmdb:+notfound+
                  (values nil nil))
                 (t
                  (unknown-error return-code))))))
      (declare (dynamic-extent #'body))
      (if key
          (with-val (raw-key key)
            (with-val (raw-value #(0))
              (body raw-key raw-value)))
          (with-empty-value (raw-key)
            (with-empty-value (raw-value)
              (body raw-key raw-value)))))))

;;; Destructors

(defun close-environment (environment)
  "Close the environment connection and free the memory.

@begin(deflist)
@term(Thread Safety)

@def(Only a single thread may call this function. All environment-dependent
objects, such as transactions and databases, must be closed before calling this
function. Attempts to use those objects are closing the environment will result
in a segmentation fault.)

@end(deflist)"
  (liblmdb:env-close (handle environment))
  (cffi:foreign-free (%handle environment)))

(defun close-database (database)
  "Close the database.

@begin(deflist)
@term(Thread Safety)

@def(Despair.

From the LMDB documentation,

@quote(This call is not mutex protected. Handles should only be closed by a
single thread, and only if no other threads are going to reference the database
handle or one of its cursors any further. Do not close a handle if an existing
transaction has modified its database. Doing so can cause misbehavior from
database corruption to errors like MDB_BAD_VALSIZE (since the DB name is
gone).))

@end(deflist)"
  (liblmdb:dbi-close (handle
                       (transaction-environment
                        (database-transaction database)))
                      (handle database))
  (cffi:foreign-free (%handle database)))

(defun close-cursor (cursor)
  "Close a cursor."
  (with-slots (database) cursor
    (liblmdb:cursor-close (handle cursor))
    (cffi:foreign-free (%handle cursor)))
  t)

;;; Macros

(defmacro with-environment ((env) &body body)
  "Open the @cl:param(env), execute @cl:param(body) and ensure the environment
is closed."
  `(progn
     (open-environment ,env)
     (unwind-protect
          (progn ,@body)
       (close-environment ,env))))

(defmacro with-database ((database) &body body)
  "Execute the body and close the database."
  `(progn
     (open-database ,database)
     (unwind-protect
          (progn ,@body)
       (close-database ,database))))

(defmacro with-cursor ((cursor) &body body)
  "Execute the body and close the cursor."
  `(progn
     (open-cursor ,cursor)
     (unwind-protect
          (progn ,@body)
       (close-cursor ,cursor))))

(defmacro do-pairs ((db key value) &body body)
  "Iterate over every key/value pair in the database."
  (let ((cur (gensym)))
    `(let ((,cur (make-cursor ,db)))
       (with-cursor (,cur)
         (multiple-value-bind (,key ,value)
             (cursor-get ,cur :first)
           (loop while ,key do
             ,@body
             (multiple-value-bind (tk tv)
                 (cursor-get ,cur :next)
               (setf ,key tk
                     ,value tv))))))))

;;; Utilities

(defun integer-to-octets (bignum &aux (n-bits (integer-length bignum)))
  (let* ((n-bytes (ceiling n-bits 8))
         (octet-vec (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8)) octet-vec))
    (loop
       :for i :from (1- n-bytes) :downto 0
       :for index :from 0
       :do (setf (aref octet-vec index) (ldb (byte 8 (* i 8)) bignum))
       :finally (return octet-vec))))

(defun version-string ()
  "Return the version string."
  (format nil "~D.~D.~D"
          liblmdb:+version-major+
          liblmdb:+version-minor+
          liblmdb:+version-patch+))
