;;;; cl-orgelctl.asd

(when (find-package :slynk) (pushnew :slynk *features*))
(when (find-package :swank) (pushnew :swank *features*))

(asdf:defsystem #:cl-orgelctl
  :description "Controller für HfMDK Orgelprojekt 2022/23"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :depends-on (#:incudine #:cm-all #:ats-cuda #:cl-orgel-gui)
  :license  "GPL 2.0 or later"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "structs")
               (:file "globals")
               (:file "orgel-accessor-fns")
               (:file "orgel-value-callbacks")
               (:file "osc")
               (:file "preset-parser")
               (:file "presets")
               (:file "utils")
               (:file "database")
               (:file "midi-cc-handler")
               (:file "midi-notein-handler")
               (:file "midi-keymap-handler")
               (:file "ats-player")
               (:file "cl-orgelctl")))
