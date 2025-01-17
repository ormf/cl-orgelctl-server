;;;; cl-orgelctl.asd

(when (find-package :slynk) (pushnew :slynk *features*))
(when (find-package :swank) (pushnew :swank *features*))

(defparameter *send-to-pd-on-orgel-server-startup* t)

;;; put the follwoing line (uncommented) into
;;; ~/.orgel-server-init.lisp if you want to replace the standard osc
;;; functions of incudine with the ones of cuda-usocket-osc:
;;;
;;; (pushnew :cuda-usocket-osc *features*)

(load "~/.orgel-server-init.lisp")

(asdf:defsystem #:cl-orgelctl-server
  :description "Controller für HfMDK Orgelprojekt 2022/23"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :depends-on (#:incudine
               ;; #:cm-all
               #:slynk #:cl-orgel-gui #:cellctl;;; #:ats-cuda
                          )
  :license  "GPL 2.0 or later"
  :version "0.0.1"
  :serial t
  :components ((:file "load-cuda-usocket-osc" :if-feature :cuda-usocket-osc)
               (:file "start-lisp-server")
               (:file "package")
               (:file "structs")
               (:file "globals")
               (:file "orgel-accessor-fns")
               (:file "orgel-value-callbacks")
               (:file "osc-server")
               (:file "preset-parser")
               (:file "presets")
               (:file "utils")
               (:file "cl-orgel-gui-redefs")
               (:file "database")
               (:file "midi-cc-handler")
               (:file "midi-notein-handler")
               (:file "midi-keymap-handler")
               (:file "osc-midi")
               (:file "cl-orgelctl-server")))
