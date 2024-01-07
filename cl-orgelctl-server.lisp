;;;; cl-orgelctl.lisp

(in-package #:cl-orgelctl)

(setf *debug* nil)


;;(cd "/home/orm/work/unterricht/frankfurt/ws_22_23/musikinformatik/papierorgel/lisp/cl-orgelctl")
(uiop:chdir (asdf:system-relative-pathname :cl-orgelctl-server ""))
(load-orgel-presets)
(load-route-presets)

;;; (permute)
;;; *midi-in1*

(start-lisp-server)
(start-osc-midi-receive)

(incudine:recv-start *midi-in1*)
(incudine:remove-all-responders *midi-in1*)
;; (make-orgel-cc-responder)
(make-orgel-note-responder)
(init-orgel-keymaps)
(start-keymap-note-responder)

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


(setup-ref-cell-hooks)
(incudine:rt-start)
(start-orgel-gui)
