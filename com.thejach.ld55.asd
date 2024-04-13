(defsystem "com.thejach.ld55"
  :description "Ludum Dare 55 entry"
  :author "Jach <jach@thejach.com>"
  :license "Public Domain/Unlicense"
  :depends-on ("lgame"
               "ironclad")
  :components ((:module "src/"
                :serial t
                :components ((:file "main")))))
