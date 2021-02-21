(mgl-pax:define-package #:lmdb
  (:documentation "See LMDB:@LMDB-MANUAL.")
  (:use #:common-lisp #:mgl-pax #:named-readtables #:pythonic-string-reader))

(mgl-pax:define-package #:lmdb+
  (:documentation "Like the LMDB package, but it has LMDB:GET fbound
  to LMDB:G3T so it probably needs shadowing to avoid conflict with
  CL:GET:

  ```
  (defpackage lmdb/test
    (:shadowing-import-from #:lmdb+ #:get)
    (:use #:cl #:lmdb))
  ```")
  (:shadow #:get)
  (:use #:cl #:lmdb)
  (:export #:get))

(cl:defpackage #:liblmdb
  (:use #:cl #:cffi)
  (:shadow :get)
  (:export :+append+
           :+appenddup+
           :+bad-dbi+
           :+bad-rslot+
           :+bad-txn+
           :+bad-valsize+
           :+corrupted+
           :+cp-compact+
           :+create+
           :+current+
           :+cursor-full+
           :+dbs-full+
           :+dupfixed+
           :+dupsort+
           :+fixedmap+
           :+incompatible+
           :+integerdup+
           :+integerkey+
           :+invalid+
           :+keyexist+
           :+last-errcode+
           :+map-full+
           :+map-resized+
           :+mapasync+
           :+multiple+
           :+nodupdata+
           :+nolock+
           :+nomeminit+
           :+nometasync+
           :+nooverwrite+
           :+nordahead+
           :+nosubdir+
           :+nosync+
           :+notfound+
           :+notls+
           :+page-full+
           :+page-notfound+
           :+panic+
           :+problem+
           :+rdonly+
           :+readers-full+
           :+reserve+
           :+reversedup+
           :+reversekey+
           :+success+
           :+tls-full+
           :+txn-full+
           :+version-date+
           :+version-full+
           :+version-major+
           :+version-minor+
           :+version-mismatch+
           :+version-patch+
           :+version-string+
           :+writemap+
           :a
           :b
           :c
           :cmp
           :constant
           :countp
           :ctx
           :cursor
           :cursor-close
           :cursor-count
           :cursor-dbi
           :cursor-del
           :cursor-get
           :cursor-op
           :cursor-open
           :cursor-put
           :cursor-renew
           :cursor-txn
           :data
           :dbi
           :dbi-close
           :dbi-flags
           :dbi-open
           :dbs
           :dcmp
           :dead
           :defanonenum
           :del
           :digit
           :drop
           :enums
           :enumvalue
           :env
           :env-close
           :env-copy
           :env-copy-2
           :env-copyfd
           :env-copyfd-2
           :env-create
           :env-get-fd
           :env-get-flags
           :env-get-maxkeysize
           :env-get-maxreaders
           :env-get-path
           :env-get-userctx
           :env-info
           :env-open
           :env-set-assert
           :env-set-flags
           :env-set-mapsize
           :env-set-maxdbs
           :env-set-maxreaders
           :env-set-userctx
           :env-stat
           :env-sync
           :envinfo
           :envinfo-tclass
           :err
           :fd
           :fix
           :flag
           :flags
           :force
           :func
           :get
           :helper
           :index
           :key
           :last
           :lower
           :lst
           :major
           :me-last-pgno
           :me-last-txnid
           :me-mapaddr
           :me-mapsize
           :me-maxreaders
           :me-numreaders
           :minor
           :mode
           :ms-branch-pages
           :ms-depth
           :ms-entries
           :ms-leaf-pages
           :ms-overflow-pages
           :ms-psize
           :mv-data
           :mv-size
           :name
           :onoff
           :op
           :package
           :parent
           :patch
           :path
           :prefix
           :put
           :reader-check
           :reader-list
           :readers
           :rel
           :rest
           :set-compare
           :set-dupsort
           :set-relctx
           :set-relfunc
           :size
           :stat
           :stat-tclass
           :strerror
           :string
           :strip-prefix
           :txn-abort
           :txn-begin
           :txn-commit
           :txn-env
           :txn-id
           :txn-renew
           :txn-reset
           :upper
           :val
           :val-tclass
           :value
           :variable
           :version)
  (:documentation "The low-level LMDB interface."))
