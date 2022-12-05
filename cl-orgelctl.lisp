;;;; cl-orgelctl.lisp

(in-package #:cl-orgelctl)

(setf *debug* nil)

(cm:cd "/home/orm/work/unterricht/frankfurt/ws_22_23/musikinformatik/papierorgel/lisp/cl-orgelctl")
(load-presets "./presets/presets.lisp")


(incudine:remove-all-responders *oscin*)

(make-all-responders *num-orgel* *oscin*)


#|
(dotimes (idx *num-orgel*)
  (make-responders idx))
|#

(let ((test (make-orgel)))
  (slot-value test 'ramp-up))

(incudine:recv-start *oscin*)

;;; (incudine.osc:close *oscout*)
;;; (incudine.osc:close *oscin*)
