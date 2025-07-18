;;;; package.lisp

(defpackage #:cl-orgelctl
  (:use #:cl #:incudine #:orm-utils #:cellctl #:cl-orgel-gui)
  (:shadowing-import-from :incudine :group)
  (:shadowing-import-from :clog :background)
  (:export #:start-orgel-server)
  )
