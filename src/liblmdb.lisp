;;;; Originally generated by SWIG 2.0.12 (http://www.swig.org), this
;;;; was incorporated from the separate LIBLMDB library into LMDB.

(in-package #:liblmdb)

(defmacro defanonenum (&body enums)
  "Converts anonymous enums to defconstants."
  `(progn ,@(loop for value in enums
                  for index = 0 then (1+ index)
                  when (listp value) do (setf index (second value)
                                                    value (first value))
                    collect `(defconstant ,value ,index))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lispify (name flag &optional (package *package*))
    (labels ((helper (lst last rest &aux (c (car lst)))
               (cond ((null lst)
                      rest)
                     ((upper-case-p c)
                      (helper (cdr lst) 'upper
                              (case last
                                ((lower digit) (list* c #\- rest))
                                (t (cons c rest)))))
                     ((lower-case-p c)
                      (helper (cdr lst) 'lower (cons (char-upcase c)
                                                     rest)))
                     ((digit-char-p c)
                      (helper (cdr lst) 'digit
                              (case last
                                ((upper lower) (list* c #\- rest))
                                (t (cons c rest)))))
                     ((char-equal c #\_)
                      (helper (cdr lst) '_ (cons #\- rest)))
                     (t
                      (error "Invalid character: ~A" c))))
             (strip-prefix (prefix string)
               (if (search prefix string)
                   (subseq string (length prefix))
                   string)))
      (let ((fix (case flag
                   ((constant enumvalue) "+")
                   (variable "*")
                   (t ""))))
        (intern
         (concatenate
          'string
          fix
          (nreverse (helper (concatenate 'list
                                         (strip-prefix "mdb_"
                                                       (strip-prefix
                                                        "MDB_" name)))
                            nil nil))
          fix)
         package)))))

(defctype size-t  :size)

(defconstant #.(lispify "MDB_VERSION_MAJOR" 'constant) 0)
(defconstant #.(lispify "MDB_VERSION_MINOR" 'constant) 9)
(defconstant #.(lispify "MDB_VERSION_PATCH" 'constant) 26)

(defconstant #.(lispify "MDB_FIXEDMAP" 'constant) #x01)
(defconstant #.(lispify "MDB_NOSUBDIR" 'constant) #x4000)
(defconstant #.(lispify "MDB_NOSYNC" 'constant) #x10000)
(defconstant #.(lispify "MDB_RDONLY" 'constant) #x20000)
(defconstant #.(lispify "MDB_NOMETASYNC" 'constant) #x40000)
(defconstant #.(lispify "MDB_WRITEMAP" 'constant) #x80000)
(defconstant #.(lispify "MDB_MAPASYNC" 'constant) #x100000)
(defconstant #.(lispify "MDB_NOTLS" 'constant) #x200000)
(defconstant #.(lispify "MDB_NOLOCK" 'constant) #x400000)
(defconstant #.(lispify "MDB_NORDAHEAD" 'constant) #x800000)
(defconstant #.(lispify "MDB_NOMEMINIT" 'constant) #x1000000)

(defconstant #.(lispify "MDB_REVERSEKEY" 'constant) #x02)
(defconstant #.(lispify "MDB_DUPSORT" 'constant) #x04)
(defconstant #.(lispify "MDB_INTEGERKEY" 'constant) #x08)
(defconstant #.(lispify "MDB_DUPFIXED" 'constant) #x10)
(defconstant #.(lispify "MDB_INTEGERDUP" 'constant) #x20)
(defconstant #.(lispify "MDB_REVERSEDUP" 'constant) #x40)
(defconstant #.(lispify "MDB_CREATE" 'constant) #x40000)

(defconstant #.(lispify "MDB_NOOVERWRITE" 'constant) #x10)
(defconstant #.(lispify "MDB_NODUPDATA" 'constant) #x20)
(defconstant #.(lispify "MDB_CURRENT" 'constant) #x40)
(defconstant #.(lispify "MDB_RESERVE" 'constant) #x10000)
(defconstant #.(lispify "MDB_APPEND" 'constant) #x20000)
(defconstant #.(lispify "MDB_APPENDDUP" 'constant) #x40000)
(defconstant #.(lispify "MDB_MULTIPLE" 'constant) #x80000)

(defconstant #.(lispify "MDB_CP_COMPACT" 'constant) #x01)

(defcenum #.(lispify "MDB_cursor_op" 'enumname)
	#.(lispify "MDB_FIRST" 'enumvalue :keyword)
	#.(lispify "MDB_FIRST_DUP" 'enumvalue :keyword)
	#.(lispify "MDB_GET_BOTH" 'enumvalue :keyword)
	#.(lispify "MDB_GET_BOTH_RANGE" 'enumvalue :keyword)
	#.(lispify "MDB_GET_CURRENT" 'enumvalue :keyword)
	#.(lispify "MDB_GET_MULTIPLE" 'enumvalue :keyword)
	#.(lispify "MDB_LAST" 'enumvalue :keyword)
	#.(lispify "MDB_LAST_DUP" 'enumvalue :keyword)
	#.(lispify "MDB_NEXT" 'enumvalue :keyword)
	#.(lispify "MDB_NEXT_DUP" 'enumvalue :keyword)
	#.(lispify "MDB_NEXT_MULTIPLE" 'enumvalue :keyword)
	#.(lispify "MDB_NEXT_NODUP" 'enumvalue :keyword)
	#.(lispify "MDB_PREV" 'enumvalue :keyword)
	#.(lispify "MDB_PREV_DUP" 'enumvalue :keyword)
	#.(lispify "MDB_PREV_NODUP" 'enumvalue :keyword)
	#.(lispify "MDB_SET" 'enumvalue :keyword)
	#.(lispify "MDB_SET_KEY" 'enumvalue :keyword)
	#.(lispify "MDB_SET_RANGE" 'enumvalue :keyword)
        #.(lispify "MDB_PREV_MULTIPLE" 'enumvalue :keyword))

(defconstant #.(lispify "MDB_SUCCESS" 'constant) 0)
(defconstant #.(lispify "MDB_KEYEXIST" 'constant) -30799)
(defconstant #.(lispify "MDB_NOTFOUND" 'constant) -30798)
(defconstant #.(lispify "MDB_PAGE_NOTFOUND" 'constant) -30797)
(defconstant #.(lispify "MDB_CORRUPTED" 'constant) -30796)
(defconstant #.(lispify "MDB_PANIC" 'constant) -30795)
(defconstant #.(lispify "MDB_VERSION_MISMATCH" 'constant) -30794)
(defconstant #.(lispify "MDB_INVALID" 'constant) -30793)
(defconstant #.(lispify "MDB_MAP_FULL" 'constant) -30792)
(defconstant #.(lispify "MDB_DBS_FULL" 'constant) -30791)
(defconstant #.(lispify "MDB_READERS_FULL" 'constant) -30790)
(defconstant #.(lispify "MDB_TLS_FULL" 'constant) -30789)
(defconstant #.(lispify "MDB_TXN_FULL" 'constant) -30788)
(defconstant #.(lispify "MDB_CURSOR_FULL" 'constant) -30787)
(defconstant #.(lispify "MDB_PAGE_FULL" 'constant) -30786)
(defconstant #.(lispify "MDB_MAP_RESIZED" 'constant) -30785)
(defconstant #.(lispify "MDB_INCOMPATIBLE" 'constant) -30784)
(defconstant #.(lispify "MDB_BAD_RSLOT" 'constant) -30783)
(defconstant #.(lispify "MDB_BAD_TXN" 'constant) -30782)
(defconstant #.(lispify "MDB_BAD_VALSIZE" 'constant) -30781)
(defconstant #.(lispify "MDB_BAD_DBI" 'constant) -30780)
(defconstant #.(lispify "MDB_LAST_ERRCODE" 'constant) -30780)

(defcstruct #.(lispify "MDB_val" 'classname)
	         (#.(lispify "mv_size" 'slotname) size-t)
	         (#.(lispify "mv_data" 'slotname) :pointer))

(defcstruct #.(lispify "MDB_stat" 'classname)
	         (#.(lispify "ms_psize" 'slotname) :uint)
	         (#.(lispify "ms_depth" 'slotname) :uint)
	         (#.(lispify "ms_branch_pages" 'slotname) size-t)
	         (#.(lispify "ms_leaf_pages" 'slotname) size-t)
	         (#.(lispify "ms_overflow_pages" 'slotname) size-t)
	         (#.(lispify "ms_entries" 'slotname) size-t))

(defcstruct #.(lispify "MDB_envinfo" 'classname)
	         (#.(lispify "me_mapaddr" 'slotname) :pointer)
	         (#.(lispify "me_mapsize" 'slotname) size-t)
	         (#.(lispify "me_last_pgno" 'slotname) size-t)
	         (#.(lispify "me_last_txnid" 'slotname) size-t)
	         (#.(lispify "me_maxreaders" 'slotname) :uint)
	         (#.(lispify "me_numreaders" 'slotname) :uint))

(defcfun ("mdb_version" #.(lispify "mdb_version" 'function)) :string
  (major :pointer)
  (minor :pointer)
  (patch :pointer))

(defcfun ("mdb_strerror" #.(lispify "mdb_strerror" 'function)) :string
  (err :int))

(defcfun ("mdb_env_create" #.(lispify "mdb_env_create" 'function)) :int
  (env :pointer))

(defcfun ("mdb_env_open" #.(lispify "mdb_env_open" 'function)) :int
  (env :pointer)
  (path :string)
  (flags :uint)
  (mode :pointer))

(defcfun ("mdb_env_copy" #.(lispify "mdb_env_copy" 'function)) :int
  (env :pointer)
  (path :string))

(defcfun ("mdb_env_copyfd" #.(lispify "mdb_env_copyfd" 'function)) :int
  (env :pointer)
  (fd :int))

(defcfun ("mdb_env_copy2" #.(lispify "mdb_env_copy2" 'function)) :int
  (env :pointer)
  (path :string)
  (flags :uint))

(defcfun ("mdb_env_copyfd2" #.(lispify "mdb_env_copyfd2" 'function)) :int
  (env :pointer)
  (fd :int)
  (flags :uint))

(defcfun ("mdb_env_stat" #.(lispify "mdb_env_stat" 'function)) :int
  (env :pointer)
  (stat :pointer))

(defcfun ("mdb_env_info" #.(lispify "mdb_env_info" 'function)) :int
  (env :pointer)
  (stat :pointer))

(defcfun ("mdb_env_sync" #.(lispify "mdb_env_sync" 'function)) :int
  (env :pointer)
  (force :int))

(defcfun ("mdb_env_close" #.(lispify "mdb_env_close" 'function)) :void
  (env :pointer))

(defcfun ("mdb_env_set_flags" #.(lispify "mdb_env_set_flags" 'function)) :int
  (env :pointer)
  (flags :uint)
  (onoff :int))

(defcfun ("mdb_env_get_flags" #.(lispify "mdb_env_get_flags" 'function)) :int
  (env :pointer)
  (flags :pointer))

(defcfun ("mdb_env_get_path" #.(lispify "mdb_env_get_path" 'function)) :int
  (env :pointer)
  (path :pointer))

(defcfun ("mdb_env_get_fd" #.(lispify "mdb_env_get_fd" 'function)) :int
  (env :pointer)
  (fd :pointer))

(defcfun ("mdb_env_set_mapsize"
          #.(lispify "mdb_env_set_mapsize" 'function)) :int
  (env :pointer)
  (size size-t))

(defcfun ("mdb_env_set_maxreaders"
          #.(lispify "mdb_env_set_maxreaders" 'function)) :int
  (env :pointer)
  (readers :uint))

(defcfun ("mdb_env_get_maxreaders"
          #.(lispify "mdb_env_get_maxreaders" 'function)) :int
  (env :pointer)
  (readers :pointer))

(defcfun ("mdb_env_set_maxdbs" #.(lispify "mdb_env_set_maxdbs" 'function)) :int
  (env :pointer)
  (dbs :uint))

(defcfun ("mdb_env_get_maxkeysize"
          #.(lispify "mdb_env_get_maxkeysize" 'function)) :int
  (env :pointer))

(defcfun ("mdb_env_set_userctx"
          #.(lispify "mdb_env_set_userctx" 'function)) :int
  (env :pointer)
  (ctx :pointer))

(defcfun ("mdb_env_get_userctx"
          #.(lispify "mdb_env_get_userctx" 'function)) :pointer
  (env :pointer))

(defcfun ("mdb_env_set_assert" #.(lispify "mdb_env_set_assert" 'function)) :int
  (env :pointer)
  (func :pointer))

(declaim (inline txn-begin))
(defcfun ("mdb_txn_begin" #.(lispify "mdb_txn_begin" 'function)) :int
  (env :pointer)
  (parent :pointer)
  (flags :uint)
  (txn :pointer))

(defcfun ("mdb_txn_env" #.(lispify "mdb_txn_env" 'function)) :pointer
  (txn :pointer))

(declaim (inline txn-id))
(defcfun ("mdb_txn_id" #.(lispify "mdb_txn_id" 'function)) size-t
  (txn :pointer))

(declaim (inline txn-commit))
(defcfun ("mdb_txn_commit" #.(lispify "mdb_txn_commit" 'function)) :int
  (txn :pointer))

(declaim (inline txn-abort))
(defcfun ("mdb_txn_abort" #.(lispify "mdb_txn_abort" 'function)) :void
  (txn :pointer))

(declaim (inline txn-reset))
(defcfun ("mdb_txn_reset" #.(lispify "mdb_txn_reset" 'function)) :void
  (txn :pointer))

(declaim (inline txn-renew))
(defcfun ("mdb_txn_renew" #.(lispify "mdb_txn_renew" 'function)) :int
  (txn :pointer))

(defcfun ("mdb_dbi_open" #.(lispify "mdb_dbi_open" 'function)) :int
  (txn :pointer)
  (name :string)
  (flags :uint)
  (dbi :pointer))

(defcfun ("mdb_stat" #.(lispify "mdb_stat" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (stat :pointer))

(defcfun ("mdb_dbi_flags" #.(lispify "mdb_dbi_flags" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (flags :pointer))

(defcfun ("mdb_dbi_close" #.(lispify "mdb_dbi_close" 'function)) :void
  (env :pointer)
  (dbi :uint))

(defcfun ("mdb_drop" #.(lispify "mdb_drop" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (del :int))

(defcfun ("mdb_set_compare" #.(lispify "mdb_set_compare" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (cmp :pointer))

(defcfun ("mdb_set_dupsort" #.(lispify "mdb_set_dupsort" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (cmp :pointer))

(defcfun ("mdb_set_relfunc" #.(lispify "mdb_set_relfunc" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (rel :pointer))

(defcfun ("mdb_set_relctx" #.(lispify "mdb_set_relctx" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (ctx :pointer))

(declaim (inline get))
(defcfun ("mdb_get" #.(lispify "mdb_get" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (key :pointer)
  (data :pointer))

(declaim (inline put))
(defcfun ("mdb_put" #.(lispify "mdb_put" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (key :pointer)
  (data :pointer)
  (flags :uint))

(declaim (inline del))
(defcfun ("mdb_del" #.(lispify "mdb_del" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (key :pointer)
  (data :pointer))

(declaim (inline cursor-open))
(defcfun ("mdb_cursor_open" #.(lispify "mdb_cursor_open" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (cursor :pointer))

(declaim (inline cursor-close))
(defcfun ("mdb_cursor_close" #.(lispify "mdb_cursor_close" 'function)) :void
  (cursor :pointer))

(declaim (inline cursor-renew))
(defcfun ("mdb_cursor_renew" #.(lispify "mdb_cursor_renew" 'function)) :int
  (txn :pointer)
  (cursor :pointer))

(defcfun ("mdb_cursor_txn" #.(lispify "mdb_cursor_txn" 'function)) :pointer
  (cursor :pointer))

(defcfun ("mdb_cursor_dbi" #.(lispify "mdb_cursor_dbi" 'function)) :uint
  (cursor :pointer))

(declaim (inline cursor-get))
(defcfun ("mdb_cursor_get" #.(lispify "mdb_cursor_get" 'function)) :int
  (cursor :pointer)
  (key :pointer)
  (data :pointer)
  (op #.(lispify "MDB_cursor_op" 'enumname)))

(declaim (inline cursor-put))
(defcfun ("mdb_cursor_put" #.(lispify "mdb_cursor_put" 'function)) :int
  (cursor :pointer)
  (key :pointer)
  (data :pointer)
  (flags :uint))

(declaim (inline cursor-del))
(defcfun ("mdb_cursor_del" #.(lispify "mdb_cursor_del" 'function)) :int
  (cursor :pointer)
  (flags :uint))

(declaim (inline cursor-count))
(defcfun ("mdb_cursor_count" #.(lispify "mdb_cursor_count" 'function)) :int
  (cursor :pointer)
  (countp :pointer))

(defcfun ("mdb_cmp" #.(lispify "mdb_cmp" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (a :pointer)
  (b :pointer))

(defcfun ("mdb_dcmp" #.(lispify "mdb_dcmp" 'function)) :int
  (txn :pointer)
  (dbi :uint)
  (a :pointer)
  (b :pointer))

(defcfun ("mdb_reader_list" #.(lispify "mdb_reader_list" 'function)) :int
  (env :pointer)
  (func :pointer)
  (ctx :pointer))

(declaim (inline reader-check))
(defcfun ("mdb_reader_check" #.(lispify "mdb_reader_check" 'function)) :int
  (env :pointer)
  (dead :pointer))
