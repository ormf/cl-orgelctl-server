;;;; cl-orgelctl.lisp

(in-package #:cl-orgelctl)

(incudine:remove-all-responders *oscin*)

(dotimes (idx *num-orgel*)
  (make-responders idx))

(let ((test (make-orgel)))
  (slot-value test 'ramp-up))

(incudine:recv-start *oscin*)

;;; (incudine.osc:close *oscout*)
;;; (incudine.osc:close *oscin*)
