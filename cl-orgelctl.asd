;;;; cl-orgelctl.asd

(asdf:defsystem #:cl-orgelctl
  :description "Controller für HfMDK Orgelprojekt 2022"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :depends-on (#:incudine #:cm-all)
  :license  "GPL 2.0 or later"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "structs")
               (:file "presets")
;;;               (:file "preset-parser")
               (:file "globals")
               (:file "osc")
               (:file "utils")
               (:file "cl-orgelctl")))
