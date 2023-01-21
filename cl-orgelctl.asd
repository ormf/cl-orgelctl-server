;;;; cl-orgelctl.asd

(asdf:defsystem #:cl-orgelctl
  :description "Controller für HfMDK Orgelprojekt 2022/23"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :depends-on (#:incudine #:cm-all)
  :license  "GPL 2.0 or later"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "structs")
               (:file "globals")
               (:file "orgel-accessor-fns")
               (:file "osc")
               (:file "preset-parser")
               (:file "presets")
               (:file "utils")
               (:file "database")
               (:file "cl-orgelctl")))
