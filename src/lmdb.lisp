;;;

(in-package :cl-user)

(defpackage lmdb
  (:use :cl)
  (:shadow :get)
  (:export
   :*database-class*
   :*debug*
   :*environment-class*
   :*transaction-class*
   :*transaction*
   :*transactions*
   :abort-transaction
   :begin-transaction
   :close-cursor
   :close-database
   :close-environment
   :commit-transaction
   :condition-environment
   :condition-name
   :cursor
   :cursor-get
   :database
   :database-maximum-count
   :database-name
   :database-not-found
   :database-statistics
   :del
   :delete-environment
   :do-pairs
   :drop-database
   :ensure-open-database
   :ensure-open-environment
   :environment
   :environment-directory
   :environment-info
   :environment-info-last-transaction-id
   :environment-statistics
   :get
   :get-with
   :lmdb-error
   :make-cursor
   :make-database
   :make-environment
   :make-transaction
   :open-cursor
   :open-database
   :open-environment
   :open-p
   :print-object-slots
   :put
   :reentrant-cursor-error
   :renew-transaction
   :reset-transaction
   :transaction
   :transaction-environment
   :transaction-parent
   :transaction-id
   :unknown-error
   :version-string
   :with-cursor
   :with-database
   :with-environment
   :with-transaction)
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

(defparameter *database-class* 'database)
(defparameter *environment-class* 'environment)
(defparameter *transaction-class* 'transaction)
(defvar *debug* nil)

(defparameter +permissions+ #o664
  "The Unix permissions to use for the database.")

;; Some error codes
(defparameter +enoent+ 2)
(defparameter +eacces+ 13)
(defparameter +eagain+ 11)

(defparameter *transactions* ()
  "Binds a list of all active transactions for reference in call-with-transaction")
(defparameter *transaction* nil
  "Binds the innermost active transaction")

;;; Classes

(defclass handle ()
  ((handle
    :accessor %handle :initarg :handle
    :documentation "The handle on respective LMDB entity.
    Depending on the concrete type, the value may be a pointer or an integer.")))

(defclass environment (handle)
  ((handle
    :documentation "The handle on the environment pointer.
    As it is used as the pointer only, it could be bound as such, but the
    handle enables conditional finalization. (see finalize-environment)")
   (directory
    :reader environment-directory
    :initarg :directory
    :documentation "The directory where environment files are stored.")
   (max-databases
    :reader environment-max-dbs
    :initarg :max-dbs :initarg :max-databases
    :initform 1
    :type integer
    :documentation "The maximum number of named databases.")
   (max-readers
    :reader environment-max-readers
    :initarg :max-readers
    :initform 1
    :type integer
    :documentation "The maximum number of threads/reader slots.")
   (mapsize
    :reader environment-mapsize
    :initarg :mapsize
    :initform (* 1024 1024) ;; rather small
    :type integer
    :documentation "The environment mapsize specifie the database size.")
   (open-flags
    :reader environment-open-flags
    :initarg :open-flags
    :initform LIBLMDB:+NOTLS+
    :type integer
    :documentation "Passed to env-open.
    The default value permits multiple threads."))
  (:documentation "Environment handle."))

(defclass transaction (handle)
  ((handle
    :documentation "The handle on the transaction pointer.")
   (env
    :reader transaction-environment
    :initarg :environment
    :type environment
    :documentation "The environment this transaction belongs to.")
   (parent
    :reader transaction-parent
    :initarg :parent :initform nil
    :type (or transaction null)
    :documentation "The parent transaction, if any.")
   (flags
    :reader transaction-flags
    :initarg :flags :initform liblmdb:+rdonly+
    :type integer
    :documentation "Flags to supply to txn_begin.
    These are supplied at initialization as, once the transaction exists,
    they cannot be changed.")
   (id
    :accessor transaction-id
    :documentation "Set to the active transaction id when begun"))   
  (:documentation "A transaction."))

(defclass database (handle)
  ((handle
    :documentation "The handle on the DBI, which is an integer.
    The initial state is unbound, to be set/cleared by open-/close-
    and tested by with-")
   (name
    :reader database-name
    :initarg :name
    :type string
    :documentation "The database name."))
  (:documentation "A database.
   The recommended practice is to open a database in a process once, in an
   initial read-only transaction, which commits. this leave the db open for
   use with later transactions.
   All the dynamic value of *transaction* governs all database operations."))

(defstruct (value (:constructor %make-value))
  "A value is a generic representation of keys and values."
  (size 0 :type fixnum)
  data)

(defclass cursor (handle)
  ((handle
    :documentation "The handle on cursor pointer.")
   (database
    :reader cursor-database
    :initarg :database :initform (error "cursor: database is required")
    :type database
    :documentation "The database the cursor belongs to.")
   (transaction
    :reader cursor-transaction
    :initarg :transaction
    :type transaction
    :documentation "Th transaction which governs the cursor.") )
  (:documentation "A cursor."))

;;; Constructors

(defun make-environment (directory &rest initargs
                                   &key
                                   (class *environment-class*)
                                   &allow-other-keys)
  "Create an environment object.

Before an environment can be used, it must be opened with @c(open-environment)."
  (declare (dynamic-extent initargs))
  (apply #'make-instance class
         :directory directory
         initargs))

(defmethod initialize-instance :before ((instance environment) &key class)
  ;; permit class initiarg
  (declare (ignore class)))

(defparameter *warn-if-parent-transactions* t)

(defgeneric make-transaction (environment &key class parent &allow-other-keys)
  (:method ((environment environment) &rest args
            &key (class *transaction-class*)
            ;; do not default to the current transaction. this is allowed
            ;; for write parents only.
            ;; (parent *transaction*)
            (parent nil)
            &allow-other-keys)
    "Create a transaction object."
    (declare (dynamic-extent args))
    (when (and *transaction* *warn-if-parent-transactions*)
      (warn "make-transaction: ~s/~s implicit parent: ~s" parent environment *transaction*))
    (apply #'make-instance class
           :environment environment
           :parent parent
           args)))

(defmethod initialize-instance ((instance transaction) &rest args &key class)
  ;; permit class initarg
  (declare (ignore class args))
  (let ((%handle (cffi:foreign-alloc :pointer)))
    (setf (cffi:mem-ref %handle :pointer) (cffi:null-pointer))
    (setf (%handle instance) %handle)
    (call-next-method)
    #+(and sbcl finalize-lmdb)
    (flet ((transaction-finalizer () (finalize-transaction %handle)))
      ;; if the environment was already closed, this is bound to fail
      (sb-ext:finalize instance #'transaction-finalizer))))

(defun make-database (name &rest initargs
                      &key (class *database-class*)
                      &allow-other-keys)
  "Create a database object.
   The initial state leaves the handle unbound (see open/close)"
  (apply #'make-instance class
         :name name
         initargs))

(defmethod initialize-instance :before ((instance database) &key class)
  ;; permit the class initarg
  (declare (ignore class)))



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


(defun make-cursor (database &key (transaction *transaction*))
  "Create a cursor object.
   The initial state leaves the handle unbound."
  (make-instance 'cursor
    :database database
    :transaction transaction))

(defmethod initialize-instance ((instance cursor) &rest args
                                &key (transaction *transaction*))
  (declare (dynamic-extent args))
  (apply #'call-next-method instance
         :transaction transaction
         args))

;;; Errors

(define-condition lmdb-error ()
  ()
  (:documentation "The base class of all LMDB errors."))

(define-condition lmdb-state-error (lmdb-error)
  ((object
    :initarg :object :initform (error "error object is required")
    :reader condition-object)))
(defun lmdb-state-error (&rest args)
  (apply #'error 'lmdb-state-error args))

(define-condition environment-error (lmdb-error)
  ((object
    :initarg :environment
    :reader condition-environment)))
(defun environment-error (&rest args)
  (apply #'error 'environment-error args))

(define-condition database-not-found (environment-error cell-error)
  ()
  (:documentation "database not found")
  (:report (lambda (condition stream)
             (format stream "Database not found, and did not specify :create t: ~s ~s."
                     (environment-directory (condition-environment condition))
                     (cell-error-name condition)))))
(defun database-not-found (&rest args)
  (apply #'error 'database-not-found args))

(define-condition database-maximum-count (environment-error)
  ()
  (:documentation "maximum database count reached")
  (:report (lambda (condition stream)
             (format stream "Reached maximum number of named databases: ~s."
                     (environment-directory (condition-environment condition))))))
(defun database-maximum-count (&rest args)
  (apply #'error 'database-maximum-count args))

(define-condition reentrant-cursor-error (lmdb-state-error)
  ((object
    :initarg :cursor
    :reader condition-cursor))
  (:documentation "The cursor given to open-cursor is already open.")
  (:report (lambda (condition stream)
             (format stream "Cursor is already open: ~s."
                     (condition-cursor condition)))))
(defun reentrant-cursor-error (&rest args)
  (apply #'error 'reentrant-cursor-error args))


(define-condition map-resized-error (environment-error)
  ()
  (:documentation "The environment has been resized.")
  (:report (lambda (condition stream)
             (format stream "Environment resized: ~s."
                     (condition-environment condition)))))
(defun map-resized-error (&rest args)
  (apply #'error 'map-resized-error args))

(defun unknown-error (error-code)
  (error "Unknown error code: ~A. Result from strerror(): ~A"
         error-code
         (liblmdb:strerror error-code)))


;;; Viscera

(defgeneric handle (object)
  (:documentation "Return the handle content from an environment, transaction or database.")
  (:method ((object handle))
    (cffi:mem-ref (%handle object) :pointer))
  (:method ((object database))
    (cffi:mem-ref (%handle object) :uint)))

(defgeneric release-handle (object)
  (:method ((object handle))
    (when (slot-boundp object 'handle)
      (cffi:foreign-free (%handle object))
      (slot-makunbound object 'handle))))

(defgeneric open-p (object)
  (:method ((object null))
    nil)
  (:method ((object handle))
    (and (slot-boundp object 'handle)
         (not (cffi:null-pointer-p (cffi:mem-ref (%handle object) :pointer)))))
  (:method ((object database))
    (slot-boundp object 'handle)))


;;; environment management
#+sbcl
(defmacro without-interrupts (&body body)
  `(sb-sys:without-interrupts ,@body))

#-sbcl
(defmacro without-interrupts (&body)
  (cerror "continue to compile" "no definition present for without-interrupts")
  `(progn ,@body))

(defmethod lmdb:print-object-slots ((object environment) stream)
  (format stream "~s"
          (if (slot-boundp object 'directory)
              (if (slot-value object 'directory)
                  (first (last (pathname-directory (slot-value object 'directory))))
                  "?")
              "?"))
  (when (slot-boundp object 'handle)
    (let ((%handle (%handle object)))
      (format stream " ~8,'0x" %handle)
      (unless (cffi:null-pointer-p %handle)
        (format stream "[~8,'0x]" (handle object))))))

(defmethod print-object ((object environment) stream)
  (let ((*print-pretty* nil))
    (print-unreadable-object (object stream :identity t :type t)
      (lmdb:print-object-slots object stream))))

(defgeneric check-for-stale-readers (environment)
  (:method ((env environment))
    (without-interrupts
      (cffi:with-foreign-object (%count :uint32)
        (let ((return-code (liblmdb:reader-check (handle env) %count)))
          (case return-code
            (0 (cffi:mem-ref %count :uint32))
            (t (unknown-error return-code))))))))

(defparameter *lmdb-verbose* t)

(defgeneric open-environment (environment &key if-does-not-exist)
  (:documentation "Open the environment connection.

@begin(deflist)
@term(Thread Safety)

@def(No special considerations.)

@end(deflist)")
  (:method :around ((environment environment) &rest args)
    (declare (dynamic-extent args) (ignore args))
    (unless (open-p environment)
      (call-next-method)))
  (:method ((environment environment) &key (create nil)
            (if-does-not-exist (if create :create :error)))
    (with-slots (directory) environment
      (assert (and (pathnamep directory) (uiop:directory-pathname-p directory)) ()
              "open-environment: invalid location: ~s" directory)
      (unless (probe-file directory)
        (ecase if-does-not-exist
          (:error (error "invalid environment location: ~s" directory))
          (:create (ensure-directories-exist directory))
          ((nil) (return-from open-environment nil))))
      (let* ((%handle (cffi:foreign-alloc :pointer)))
        (case (liblmdb:env-create %handle)
          (0 (let ((%environment (cffi:mem-ref %handle :pointer)))
               (liblmdb:env-set-maxdbs %environment (environment-max-dbs environment))
               (liblmdb:env-set-mapsize %environment (environment-mapsize environment))
               (liblmdb:env-set-maxreaders %environment (environment-max-readers environment))
               (let ((return-code
                      (liblmdb:env-open %environment
                                        (namestring directory)
                                        (environment-open-flags environment)
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
                                     ;; Success: bind the environment and configure it
                                     (setf (%handle environment) %handle)
                                     #+sbcl
                                     (sb-ext:finalize environment
                                                      #'(lambda () (finalize-environment %handle)))
                                     (when *lmdb-verbose*
                                       (format *trace-output* "~&open environment ~a~%" environment))
                                     t)
                                    (t
                                     (unknown-error return-code))))))
          (t
           (error "Error creating environment object.")))))
    (check-for-stale-readers environment)
    environment))

(defun finalize-environment (%handle)
  "When an environment instance is no longer reachable, examine its
 lmdb environment handle. Iff that is not null, the environment was never closed - close it.
 Finally, free the handle"
  (let ((%env (cffi:mem-ref %handle :pointer)))
    (unless (cffi:null-pointer-p %env)
      (when *lmdb-verbose*
        (format *trace-output* "~&finalize environment ~8,'0x[~8,'0x]~%" %handle %env))
      ;; to be sure
      (setf (cffi:mem-ref %handle :pointer) (cffi:null-pointer))
      ;; then close it
      (liblmdb:env-close %env)
      (cffi:foreign-free %handle))))

(defun close-environment (environment)
  "Close the environment connection and free the memory.

@begin(deflist)
@term(Thread Safety)

@def(Only a single thread may call this function. All environment-dependent
objects, such as transactions and databases, must be closed before calling this
function. Attempts to use those objects are closing the environment will result
in a segmentation fault.)

@end(deflist)"
  (when (slot-boundp environment 'handle)
    ;; similar to finalize, but do not free the handle.
    (let ((%env (handle environment)))
      (unless (cffi:null-pointer-p %env)
        (when *lmdb-verbose*
          (format *trace-output* "~&close environment ~a~%" environment))
        (liblmdb:env-close %env)
        (setf (cffi:mem-ref (%handle environment) :pointer) (cffi:null-pointer))))
    ;;!! this eliminates the reference, but leaves the handle allocated to be
    ;; available to the finalize-environment operator
    (slot-makunbound environment 'handle))
  t)

(defun delete-environment (environment)
  (let ((directory (environment-directory environment)))
    (uiop:delete-directory-tree directory :validate 'probe-file)))

(defun environment-statistics (environment)
  "Return statistics about the environment."
  (cffi:with-foreign-object (stat '(:struct liblmdb:stat))
    (liblmdb:env-stat (handle environment)
                       stat)
    (macrolet ((slot (slot)
                 `(cffi:foreign-slot-value stat
                                           '(:struct liblmdb:stat)
                                           ',slot)))
      (list :page-size (slot liblmdb:ms-psize)
            :depth (slot liblmdb:ms-depth)
            :branch-pages (slot liblmdb:ms-branch-pages)
            :leaf-pages (slot liblmdb:ms-leaf-pages)
            :overflow-pages (slot liblmdb:ms-overflow-pages)
            :entries (slot liblmdb:ms-entries)
            ))))

(defun environment-info (environment)
  "Return information about the environment."
  (cffi:with-foreign-object (info '(:struct liblmdb:envinfo))
    (liblmdb:env-info (handle environment)
                       info)
    (macrolet ((slot (slot)
                 `(cffi:foreign-slot-value info
                                           '(:struct liblmdb:envinfo)
                                           ',slot)))
      (list :map-address (slot liblmdb:me-mapaddr)
            :map-size (slot liblmdb:me-mapsize)
            :last-page-number (slot liblmdb:me-last-pgno)
            :last-transaction-id (slot liblmdb:me-last-txnid)
            :maximum-readers (slot liblmdb:me-maxreaders)
            :number-of-readers (slot liblmdb:me-numreaders)))))

(defun environment-info-last-transaction-id (environment)
  "Return information about the environment."
  (cffi:with-foreign-object (info '(:struct liblmdb:envinfo))
    (liblmdb:env-info (handle environment) info)
    (macrolet ((slot (slot)
                 `(cffi:foreign-slot-value info
                                           '(:struct liblmdb:envinfo)
                                           ',slot)))
      (slot liblmdb:me-last-txnid))))


;;; transaction management

(defmethod print-object ((object transaction) stream)
  (let ((*print-pretty* nil))
    (print-unreadable-object (object stream :identity t :type t)
      (format stream "~s@~s"
              (if (slot-boundp object 'env)
                  (slot-value object 'env)
                  "?")
              (if (slot-boundp object 'id)
                  (slot-value object 'id)
                              "-")))))

(defun finalize-transaction (%handle)
  "When a transaction instance is no longer reachable, examine its
 lmdb transaction handle. Iff that is not null, the transaction is still open, abort it.
 Finally, always free the handle"
  (let ((%txn (cffi:mem-ref %handle :pointer)))
    (unless (cffi:null-pointer-p %txn)
      ;; to be sure
      (setf (cffi:mem-ref %handle :pointer) (cffi:null-pointer))
      ;; then close it
      (liblmdb:txn-abort %txn))
    (cffi:foreign-free %handle)))
                                     
(defparameter +map-resized-limit+ 3)

(defgeneric begin-transaction (transaction &key flags)
  (:documentation "Begin the transaction.

@begin(deflist)
@term(Thread Safety)

@def(A transaction may only be used by a single thread, unless thread-local storage
 when the transaction is created.
 Moreover, a given thread should use ony transaction only and
 nesting is permitted for parent write transactions only.)

@end(deflist)")
  (:method ((transaction transaction) &key (flags (transaction-flags transaction)))
    (with-slots (env parent) transaction
      (let ((%handle (%handle transaction)))
        (when (cffi:null-pointer-p (cffi:mem-ref %handle :pointer))
          (loop for retry-count below +map-resized-limit+
            do (let ((return-code
                      (without-interrupts
                        (liblmdb:txn-begin (handle env)
                                           (if parent
                                               (%handle parent)
                                               (cffi:null-pointer))
                                           flags
                                           %handle))))
                 (alexandria:switch (return-code)
                                    (0
                                     ;; record the current transaction id and...
                                     (let ((%txn (cffi:mem-ref %handle :pointer)))
                                       (setf (transaction-id transaction)
                                             (cffi:foreign-slot-value %txn '(:struct liblmdb:txn) 'liblmdb:mt-txnid)))
                                     ;; ... return success
                                     (return t))
                                    (liblmdb:+map-resized+
                                     (when *lmdb-verbose*
                                       (format *trace-output* "~&map resized attempt ~a: ~a~%" retry-count env))
                                     ;; resize the map and try again
                                     (liblmdb:env-set-mapsize (handle env) 0))
                                    ;; TODO rest
                                    (t
                                     (unknown-error return-code))))
            finally (map-resized-error :environment env)))))))


(defun require-open-transaction (transaction &optional (message nil))
  (assert (open-p transaction) ()
          "~@[~a: ~]transaction not open: ~s." message transaction))


(defgeneric commit-transaction (transaction)
  (:documentation "Commit the transaction. The transaction pointer is freed.

@begin(deflist)
@term(Thread Safety)

@def(The LMDB documentation doesn't say specifically, but assume it can only be
called by the transaction-creating thread.)

@end(deflist)")
  (:method ((transaction transaction))
    ;; multiple commits are an error
    (require-open-transaction transaction "commit-transaction")
    (let ((%handle (%handle transaction))
          (%txn (handle transaction)))
      (setf (cffi:mem-ref %handle :pointer) (cffi:null-pointer))
      (let ((return-code (liblmdb:txn-commit %txn)))
        (alexandria:switch (return-code :test #'=)
          (0
           ;; Success
           t)
          (t
           (unknown-error return-code)))))))


(defgeneric abort-transaction (transaction)
  (:documentation "Abort the transaction. The transaction pointer is freed.

@begin(deflist)
@term(Thread Safety)

@def(The LMDB documentation doesn't say specifically, but assume it can only be
called by the transaction-creating thread.)

@end(deflist)")
  (:method ((transaction transaction))
    ;; multiple aborts have no effect
    (when (open-p transaction)
      ;; allow multiple threads to close with impugnity.
      ;; nb. provide for thread-safety elsewhere
      (let ((%handle (%handle transaction))
            (%txn (handle transaction)))
        (setf (cffi:mem-ref %handle :pointer) (cffi:null-pointer))
        ;; if interrupted, this will leak the transaction memory
        (liblmdb:txn-abort %txn)))))


(defgeneric renew-transaction (transaction)
  (:documentation "Renew the transaction.

@begin(deflist)
@term(Thread Safety)

@def(A transaction may only be used by a single thread.)

@end(deflist)")
  (:method ((transaction transaction))
    (with-slots (env parent) transaction
      (let ((return-code
             (without-interrupts
               (liblmdb:txn-renew (handle transaction)))))
        (alexandria:switch (return-code)
          (0
           ;; Success
           t)
          ;; TODO rest
          (t
           (unknown-error return-code)))))))


(defgeneric reset-transaction (transaction)
  (:documentation "Reset the transaction.

@begin(deflist)
@term(Thread Safety)

@def(A transaction may only be used by a single thread.)

@end(deflist)")
  (:method ((transaction transaction))
    (require-open-transaction transaction "reset-transaction")
    (liblmdb:txn-reset (handle transaction))))


(defgeneric enter-transaction (transaction disposition)
  (:documentation "Either begin or renew the transaction, as per disposition.")
  (:method ((transaction lmdb:transaction) (disposition (eql :begin)))
    (begin-transaction transaction :flags (transaction-flags transaction)))
  (:method ((transaction lmdb:transaction) (disposition (eql :renew)))
    (renew-transaction transaction))
  (:method ((transaction lmdb:transaction) (disposition (eql :continue)))
    (require-open-transaction transaction "enter-transaction(:continue)")
    transaction))

(defgeneric leave-transaction (transaction disposition)
  (:method ((transaction transaction) (disposition (eql :abort)))
    (abort-transaction transaction))
  (:method ((transaction transaction) (disposition (eql :commit)))
    (commit-transaction transaction))
  (:method ((transaction transaction) (disposition (eql :reset)))
    (reset-transaction transaction))
  (:method ((transaction lmdb:transaction) (disposition (eql :continue)))
    transaction))


;;; database management

(defmethod print-object ((object database) stream)
  (let ((*print-pretty* nil))
    (print-unreadable-object (object stream :identity t :type t)
      (format stream "~s" (if (slot-boundp object 'name)
                              (slot-value object 'name)
                              "?")))))

(defgeneric open-database (database &key transaction if-does-not-exist)
  (:documentation "Open a database.
Bind the dbi handle and call dbi-open to set it.
@begin(deflist)
@term(Thread Safety)

@def(A transaction that opens a database must finish (either commit or abort)
before another transaction may open it. Multiple concurrent transactions cannot
open the same database.)

@end(deflist)")
  
  (:method ((database database) &key (transaction *transaction*) (if-does-not-exist :error))
    (with-slots (name) database
      (require-open-transaction transaction "open-database")
      (when (open-p database)
        (warn "open-database: reentrant invocation: ~s ~s." database transaction))
      (unless (slot-boundp database 'handle)
        ;; dbi-open claims to be idempotent, but anyway
        (let* ((%handle (cffi:foreign-alloc :uint))
               (return-code (liblmdb:dbi-open (handle transaction)
                                              name
                                              (case if-does-not-exist
                                                (:create liblmdb:+create+)
                                                (t 0))
                                              %handle)))
          (alexandria:switch (return-code)
            (0
             ;; Success
             (setf (%handle database) %handle)
             (values database
                     (handle database)))
            (liblmdb:+notfound+
             (ecase if-does-not-exist
               (:error
                (database-not-found :name name :environment (transaction-environment transaction)))
               ((nil)
                nil)))
            (liblmdb:+dbs-full+
             (database-maximum-count :name name :environment (transaction-environment transaction)))
            (t
             (unknown-error return-code))))))))

(defgeneric close-database (database &key transaction)
  (:documentation
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

@end(deflist)")
  (:method ((database database) &key (transaction *transaction*))
    (with-slots (name) database
      (liblmdb:dbi-close (handle (transaction-environment transaction))
                         (handle database))
      (release-handle database)
      t)))

(defgeneric drop-database  (database &key delete transaction)
  (:method ((database database) &key (delete 0) (transaction *transaction*))
    (require-open-transaction transaction "drop-database")
    (liblmdb:drop (handle transaction) (handle database) delete)))

(defun database-statistics (database &key (transaction *transaction*))
  "Return statistics about the database."
  (cffi:with-foreign-object (%stat '(:struct liblmdb:stat))
    (require-open-transaction transaction "database-statistics")
    (liblmdb:stat (handle transaction)
                      (handle database)
                      %stat)
    (macrolet ((slot (slot)
                 `(cffi:foreign-slot-value %stat
                                           '(:struct liblmdb:stat)
                                           ',slot)))
      (list :page-size (slot liblmdb:ms-psize)
            :depth (slot liblmdb:ms-depth)
            :branch-pages (slot liblmdb:ms-branch-pages)
            :leaf-pages (slot liblmdb:ms-leaf-pages)
            :overflow-pages (slot liblmdb:ms-overflow-pages)
            :entries (slot liblmdb:ms-entries)
            ))))

;;; cursor operations

(defun open-cursor (cursor)
  "Open a cursor."
  (when (slot-boundp cursor 'handle)
    (reentrant-cursor-error :cursor cursor))
  (with-slots (database transaction) cursor
    (require-open-transaction transaction "open-cursor")
    (assert (open-p database) ()
            "open-cursor: database not active: ~s ~s." database transaction)
    (let* ((%handle (cffi:foreign-alloc :pointer))
           (return-code (liblmdb:cursor-open (handle transaction)
                                             (handle database)
                                             %handle)))
      (alexandria:switch (return-code)
        (0
         ;; Success
         (setf (%handle cursor) %handle))
        (22
         (error "Invalid parameter."))
        (t
         (unknown-error return-code)))))
  cursor)

(defun close-cursor (cursor)
  "Close a cursor."
  (liblmdb:cursor-close (handle cursor))
  (cffi:foreign-free (%handle cursor))
  (slot-makunbound cursor 'handle)
  t)

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

(defun get (database key &key (transaction *transaction*))
  "Get a value from the database."
  ;;!! no trnsaction state check
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
           (unknown-error return-code)))))))

(defun put (database key value &key (transaction *transaction*))
  "Add a value to the database."
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
           (unknown-error return-code))))))
  value)

(defun del (database key data &key (transaction *transaction*))
  "Delete this key from the database. Returns @c(t) if the key was found,
@c(nil) otherwise."
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
         (unknown-error return-code))))))

(defun cursor-get (cursor operation)
  "Extract data using a cursor.

The @cl:param(operation) argument specifies the operation."
  (let ((op (case operation
              (:first :+first+)
              (:current :+current+)
              (:last :+last+)
              (:next :+next+)
              (:prev :+prev+))))
    (with-empty-value (raw-key)
      (with-empty-value (raw-value)
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
             (unknown-error return-code))))))))

(defun cursor-get-with (cursor operation continuation)
  "Extract data using a cursor with a decoding continuation.

The @cl:param(operation) argument specifies the operation."
  (declare (dynamic-extent continuation))
  (let ((op (case operation
              (:first :+first+)
              (:current :+current+)
              (:last :+last+)
              (:next :+next+)
              (:prev :+prev+)
              (:set-range :+set-range+))))
    (with-empty-value (raw-key)
      (with-empty-value (raw-value)
        (let ((return-code (liblmdb:cursor-get (handle cursor)
                                                raw-key
                                                raw-value
                                                op)))
          (alexandria:switch (return-code)
            (0
             ;; Success
             (funcall continuation raw-key raw-value))
            (liblmdb:+notfound+
             (values nil nil))
            (t
             (unknown-error return-code))))))))

(defun get-with (database key operator &key (transaction *transaction*))
  "Get a value from the database with a decoding continuation."
  (with-val (raw-key key)
    (with-empty-value (raw-value)
      (let ((return-code (liblmdb:get (handle transaction)
                                      (handle database)
                                      raw-key
                                      raw-value)))
        (alexandria:switch (return-code)
          (0
           ;; Success
           (funcall operator raw-key raw-value))
          (liblmdb:+notfound+
           (values nil nil))
          (t
           (unknown-error return-code)))))))


;;; Macros

(defun call-with-open-environment (op environment &rest args)
  (declare (dynamic-extent op))
  (cond ((open-p environment)
         (funcall op environment))
        (t
         (apply #'open-environment environment args)
         (unwind-protect
             (funcall op environment)
           (close-environment environment)))))

(defmacro with-environment ((environment &rest args) &body body)
  "Execute the @cl:param(body) in a context which ensures that the @cl:param(environment) is open.
 If that is already the case, do not change the state.
 If open was necessary, close the environment upon conclusion."
  (let ((op (gensym "with-environment-body-"))
        (environment-variable (etypecase environment
                                (cons (first environment))
                                (symbol environment)))
        (environment-form (if (consp environment)
                              (if (consp (second environment))
                                  (second environment)
                                  `(make-environment ,@(rest environment)))
                              environment)))
    `(flet ((,op (,environment-variable)
              (declare (ignorable ,environment-variable))
              ,@body))
       (declare (dynamic-extent #',op))
       (call-with-open-environment #',op ,environment-form ,@args))))

(defun call-ensuring-open-environment (op environment)
  (declare (dynamic-extent op))
  (unless (open-p environment)
    (open-environment environment))
  (funcall op))

(defmacro ensure-open-environment ((environment) &body body)
  "Execute the @cl:param(body) in a context which ensures that the @cl:param(environment) is open.
 If that is already the case, do not change the state.
 In any case, leave the environment open upon conclusion"
  (let ((op (gensym "ensure-environment-body-")))
    `(flet ((,op () ,@body))
       (declare (dynamic-extent #',op))
       (call-ensuring-open-environment #',op ,environment))))


(defun funcall-transaction-op (op transaction)
  (funcall op transaction))

(defun call-with-transaction (op transaction
                                 &key
                                 (normal-disposition :abort)
                                 (error-disposition :abort)
                                 (initial-disposition :begin))
  "If the transaction is already established, just call the operator.
 Otherwise, wrap that call with bindings for the current and nested
 transaction, instantiate the transaction and track completion when
 closing it.
 NB, establishment is presence in a dynamic context, not wheter it is open.
 an open transaction may be shared between threads - that is, opened in one
 and involved in operations in another."
  (declare (dynamic-extent op))
  (let ((existing nil))
    (cond ((find transaction *transactions*)
           ;; this indicate a probable program error: a transaction wrapper should not nest in itself.
           ;; especially with nested write transactions, this is actually not legal:
           ;; it implies operations on entities governed by the transaction, but
           ;; as per http://www.lmdb.tech/doc/group__mdb.html#gad7ea55da06b77513609efebd44b26920
           ;; "A parent transaction and its cursors may not issue any other operations than mdb_txn_commit and mdb_txn_abort while it has active child transactions"
           (warn "lmdb:call-with-transaction: intention to operate on parent transaction ~s/~s"
                 *transaction* transaction)
           (funcall-transaction-op op transaction))
          ((and (setf existing (find (transaction-environment transaction) *transactions* :key #'transaction-environment))
                (logior (transaction-flags transaction) liblmdb:+rdonly+))
           ;; if it is read-only and a governing one exists, do not nest, just use _the existing one_
           ;; rebinding the global value to the existing one.
           (when *lmdb-verbose*
             (format *trace-output* "~&reuse governing transaction ~s instead of ~s~@[ shadowing ~s~]~%" 
                     existing transaction (unless (eq *transaction* existing) *transaction*)))
           (let ((*transaction* existing))
             (funcall-transaction-op op existing)))
          (t
           (let ((status nil)
                 (*transactions* (cons transaction *transactions*))
                 (*transaction* transaction))
             (declare (dynamic-extent *transactions*))
             (enter-transaction transaction initial-disposition)
             (unwind-protect (handler-bind ((serious-condition (lambda (c) (setf status c)))) ;; no control transfer
                               (multiple-value-prog1 (funcall-transaction-op op transaction)
                                 (leave-transaction transaction normal-disposition)
                                 (setf status normal-disposition)))
               (unless (eq status normal-disposition)
                 ;; distinguish an aysnchronous termination for the operation
                 ;; from an an error
                 (when (and *debug* (typep status 'serious-condition))
                   (warn "lmdb:call-with-transaction leave transaction in unwind: ~s ~s: ~a"
                         transaction (cffi:mem-ref (%handle transaction) :pointer) status))
                 (leave-transaction transaction error-disposition))))))))

(defmacro with-transaction ((transaction &rest options
                                         &key normal-disposition
                                         error-disposition
                                         initial-disposition)
                            &body body)
  (declare (ignore normal-disposition error-disposition initial-disposition))
  (let ((op (gensym))
        (transaction-variable (etypecase transaction
                                (cons (first transaction))
                                (symbol transaction)))
        (transaction-form (if (consp transaction)
                              (if (consp (second transaction))
                                  (second transaction)
                                  `(make-transaction ,@(rest transaction)))
                              transaction)))
    `(flet ((,op (,transaction-variable)
              (declare (ignorable ,transaction-variable))
              ,@body))
       (declare (dynamic-extent #',op))
       (call-with-transaction #',op ,transaction-form ,@options))))


(defgeneric call-with-open-database (op database)
  (:method ((op function) (database database))
    (declare (dynamic-extent op))
    (cond ((open-p database)
           (funcall op))
          (t
           (open-database database)
           (unwind-protect
               (funcall op)
             (close-database database))))))

(defmacro with-database ((database) &body body)
  "Execute the body in a context which ensures that the database is open.
 If that is already the case, do not change the state.
 If open was necessary, close the database upon conclusion."
  (let ((op (gensym "with-database-body-")))
    `(flet ((,op () ,@body))
       (declare (dynamic-extent #',op))
       (call-with-open-database #',op ,database))))

(defun ensure-open-database (database)
  (unless (open-p database)
    (open-database database))
  database)

(defgeneric call-with-open-cursor (op cursor)
  (:documentation "Open a cursor for the extent of a function call")
  (:method ((op function) (cursor cursor))
    (open-cursor cursor)
    (unwind-protect
        (funcall op)
      (close-cursor cursor))))

(defmacro with-cursor ((cursor) &body body)
  "Execute the body and close the cursor."
  (let ((op (gensym "with-cursor-body-")))
    `(flet ((,op () ,@body))
       (declare (dynamic-extent #',op))
       (call-with-open-cursor #',op ,cursor))))

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

(defmacro do-pairs-with ((db key value operator) &body body)
  "Iterate over every key/value pair in the database, as retrieved with the provided operator."
  (let ((cur (gensym)))
    `(let ((,cur (make-cursor ,db)))
       (with-cursor (,cur)
         (multiple-value-bind (,key ,value)
             (cursor-get-with ,cur :first ,operator)
           (loop while ,key do
             ,@body
             (multiple-value-bind (tk tv)
                 (cursor-get-with ,cur :next ,operator)
               (setf ,key tk
                     ,value tv))))))))

;;; Utilities

;;; this is a provisional operators to compute trivial keys.
;;; it includes some logic to ensure stable key lengths, but amore stable method is
;;; to use a structure with the appropriately typed field(s).

(defun integer-to-octets (bignum)
  "Return a byte vector padded to standard integer sizes when they fit,
 that is for 4 and 8 byte words, or variable above that.
 The standard case is to ensure compatibility with values stored as
 native word and long integers in other languages, _but_ in little-endian form
 only."
  (let* ((n-bits (integer-length bignum))
         (n-bytes-exact (ceiling n-bits 8))
         (n-bytes (cond ((> n-bytes-exact 8) n-bytes-exact)
                        ((> n-bytes-exact 4) 8)
                        (t 4)))
         (octet-vec (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8)) octet-vec))
    (loop
       :for index  :from (1- n-bytes) :downto 0
       :do (setf (aref octet-vec index) (ldb (byte 8 (* index 8)) bignum))
       :finally (return octet-vec))))

(defun version-string ()
  "Return the version string."
  (format nil "~D.~D.~D"
          liblmdb:+version-major+
          liblmdb:+version-minor+
          liblmdb:+version-patch+))
