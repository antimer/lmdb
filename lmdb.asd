(asdf:defsystem lmdb
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/antimer/lmdb"
  :bug-tracker "https://github.com/antimer/lmdb/issues"
  :source-control (:git "git@github.com:antimer/lmdb.git")
  :depends-on (:liblmdb
               :alexandria
               :trivial-utf-8)
  :components ((:module "src"
                :components
                ((:file "lmdb"))))
  :description "Bindings to LMDB."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op lmdb-test))))
