(asdf:defsystem lmdb-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:lmdb
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "lmdb")))))
