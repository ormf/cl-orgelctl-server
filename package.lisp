;;;; package.lisp

(defpackage #:cl-orgelctl
  (:shadowing-import-from :clog :background)
  (:shadowing-import-from :incudine :group)
  (:use #:cl #:incudine #:orm-utils #:cellctl #:cl-orgel-gui)
  (:export #:start-orgel-server)
  )
