(in-package :cl-user)
(defpackage lmdb-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :lmdb-test)

(def-suite tests
  :description "lmdb tests.")
(in-suite tests)

(defparameter +env-directory+
  (asdf:system-relative-pathname :lmdb #p"t/env/"))

(defun clean ()
  (uiop:delete-directory-tree +env-directory+ :validate t))

(test environment
  (let ((env))
    (finishes
      (setf env (lmdb:make-environment +env-directory+)))
    (finishes
      (lmdb:open-environment env))
    (finishes
      (lmdb:environment-statistics env))
    (finishes
      (lmdb:environment-info env))
    (finishes
      (lmdb:close-environment env)))
  (clean))

(test database
  (let ((env (lmdb:make-environment +env-directory+)))
    (lmdb:with-environment (env)
      (let ((txn))
        (finishes
          (setf txn (lmdb:make-transaction env)))
        (finishes
          (lmdb:begin-transaction txn))
        (let ((db))
          (finishes
            (setf db (lmdb:make-database txn "db"
                                         :if-does-not-exist :create)))
          (is-true
           (lmdb::database-create-p db))
          (finishes
            (lmdb:open-database db))
          (finishes
            (lmdb:close-database db))))))
  (clean))

(test values
  (loop for val in (list #(1)
                         #(1 2 3)
                         #(35 2 34 23))
        do
    (let ((value (lmdb::make-value val)))
      (is-true
       (lmdb::value-p value))
      (is-true
       (integerp (lmdb::value-size value)))
      (is-true
       (vectorp (lmdb::value-data value)))
      (lmdb::with-val (raw-val val)
        (is
         (equal (cffi:pointer-address
                 (cffi:foreign-slot-value raw-val
                                          '(:struct liblmdb:val)
                                          'liblmdb:mv-size))
                (lmdb::value-size value)))
        (loop for i from 0 to (1- (lmdb::value-size value)) do
          (is
           (equal (cffi:mem-aref (cffi:foreign-slot-value raw-val
                                                          '(:struct liblmdb:val)
                                                          'liblmdb:mv-data)
                                 :unsigned-char
                                 i)
                  (elt (lmdb::value-data value) i))))))))

(defmacro with-query ((env db db-name) &body body)
  (let ((txn (gensym)))
    `(let ((,txn (lmdb:make-transaction ,env)))
       (is-true
        (lmdb:begin-transaction ,txn))
       (let ((,db (lmdb:make-database ,txn ,db-name)))
         (lmdb:with-database (,db)
           ,@body
           (is-true
            (lmdb:commit-transaction ,txn)))))))

(test queries
  (let ((env (lmdb:make-environment +env-directory+)))
    (lmdb:with-environment (env)
      (with-query (env db "db")
        (finishes
          (lmdb:put db #(1) #(2)))
        (finishes
          (lmdb:put db #(2) #(3)))
        (finishes
          (lmdb:put db #(3) #(4)))
        (is
         (equalp (lmdb:get db #(1))
                 #(2)))
        (is
         (equalp (lmdb:get db #(2))
                 #(3)))
        (is
         (equalp (lmdb:get db #(3))
                 #(4))))
      (with-query (env db "db")
        (is
         (equalp (lmdb:get db #(1))
                 #(2)))
        (is
         (equalp (lmdb:get db #(2))
                 #(3)))
        (is
         (equalp (lmdb:get db #(3))
                 #(4))))
      (with-query (env db "db")
        (is-true
         (lmdb:del db #(1))))
      (with-query (env db "db")
        (is
         (null (lmdb:get db #(1))))
        (finishes
          (lmdb:put db #(1) #(2)))))))

(test cursors
  (let ((env (lmdb:make-environment +env-directory+)))
    (lmdb:with-environment (env)
      (let ((txn (lmdb:make-transaction env)))
        (is-true
         (lmdb:begin-transaction txn))
        (let ((db (lmdb:make-database txn "db")))
          (lmdb:with-database (db)
            (let ((cur (lmdb:make-cursor db)))
              (lmdb:with-cursor (cur)
                (multiple-value-bind (key value)
                    (lmdb:cursor-get cur :first)
                  (is
                   (typep key 'vector))
                  (is
                   (typep value 'vector)))
                (multiple-value-bind (key value)
                    (lmdb:cursor-get cur :next)
                  (is
                   (typep key 'vector))
                  (is
                   (typep value 'vector))))
              (lmdb:commit-transaction txn))))))))

(test iteration
  (let ((env (lmdb:make-environment +env-directory+)))
    (lmdb:with-environment (env)
      (let ((txn (lmdb:make-transaction env)))
        (is-true
         (lmdb:begin-transaction txn))
        (let ((db (lmdb:make-database txn "db")))
          (lmdb:with-database (db)
            (let ((count 0))
              (lmdb:do-pairs (db key value)
                (is
                 (typep key 'vector))
                (is
                 (typep value 'vector))
                (incf count))
              (is
               (= count 3)))))
        (is-true
         (lmdb:commit-transaction txn))))))

(defun run-tests ()
  (unwind-protect
       (dotimes (i 20)
         (run! 'tests))
    (uiop:delete-directory-tree +env-directory+ :validate t)))
