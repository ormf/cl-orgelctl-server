;;;; cl-orgelctl.lisp

(in-package #:cl-orgelctl)

(setf *debug* nil)


;;(cd "/home/orm/work/unterricht/frankfurt/ws_22_23/musikinformatik/papierorgel/lisp/cl-orgelctl")
(uiop:chdir (asdf:system-relative-pathname :cl-orgelctl-server ""))
(load-orgel-presets)
(load-route-presets)

;;; (permute)
;;; *midi-in1*

;;; (js-execute msl (format nil "multislider(~A, { \"colors\": ~a, \"thumb\": '~(~a~)'})" (jquery msl) data-colors (if thumb "true" "false")))

;; (make-orgel-cc-responder)
(init-orgel-keymaps)

;;; (init-orgel-keymaps)
;;; (stop-keymap-note-responder)
;;; (start-keymap-note-responder)
;;; (print-pending-keymap-responders)
;;; (clear-keymap-responders)

#|
(dotimes (idx *orgelcount*)
  (make-responders idx))

(let ((test (make-orgel)))
  (slot-value test 'ramp-up))
|#

;;; (incudine:recv-start *oscin*)

;;; (incudine.osc:close *oscout*)
;;; (incudine.osc:close *oscin*)

(defun start-orgel-server (&key (send-to-pd t))
  (let ((host (or (cl-user::create-slynk-server) "127.0.0.1")))
    (if *midi-in1* (incudine:remove-all-responders *midi-in1*))
    (start-midi)
    (make-orgel-note-responder)
    (start-keymap-note-responder)
    (incudine:recv-start *midi-in1*)
    (start-osc-midi-receive)
    (start-lisp-server :local-host host :send-to-pd send-to-pd)
    (incudine:rt-start)
    (sleep 2)
    (start-orgel-gui)
    (setf *package* (find-package :cl-orgelctl))
    (uiop:chdir (asdf:system-relative-pathname :cl-orgelctl-server ""))))

(defun orgel-setup ()
  (setup-ref-cell-hooks))

;;; initialization:

(orgel-setup)

(in-package :cl-user)

(defun papierrohrorgel-start (&key (send-to-pd t))
  (setf *package* (find-package :cl-orgelctl))
  (cl-orgelctl:start-orgel-server :send-to-pd send-to-pd)
  (setf *package* (find-package :cl-orgelctl)))

(export 'papierrohrorgel-start 'cl-user)

(papierrohrorgel-start :send-to-pd cl-user::*send-to-pd-on-orgel-server-startup*)
