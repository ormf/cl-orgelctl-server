;;;; cl-orgelctl.lisp

(in-package #:cl-orgelctl)

(setf *debug* nil)
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
