;;; 
;;; orgel-value-callbacks.lisp
;;;
;;; callback functions when a value changes (triggered by osc messages
;;; or by fader movements in the html gui).
;;;
;;; the callback does 3 things:
;;;
;;; 1. update the respective slot of *curr-state*
;;; 2. set the faders in the html gui
;;; 3. set the values on the pd side
;;;
;;; the src argument is nil if the values are received from osc and
;;; therefore the callbacks don't send to pd (another mechanism has to
;;; be implemented if the patch should also receive osc messages from
;;; other sources than the pd patch).
;;;
;;; the src argument otherwise contains the html element which caused
;;; the value change to prevent a loop in the value changes to the
;;; connected html gui instances.
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-orgelctl)

(defun orgel-global-value-callback (orgelidx target value src)
  (declare (ignore src))
  (setf (slot-value (aref *curr-state* orgelidx) target) value)
  (format t "g-v-c, ~S ~S ~S" orgelidx target value)
  (orgel-ctl (orgel-name (1+ orgelidx)) target value)
  (mapcar #'funcall (slot-value (aref *osc-responder-registry* orgelidx) target)))

;;; (orgel-ctl (orgel-name 1) :base-freq 28)

(defun orgel-fader-value-callback (orgelidx target faderidx value src)
  (declare (ignore src))
  (let ((f-idx (round (1- faderidx))))
;;;    (break "orgelidx: ~a, target: ~a ,faderidx: ~a, value: ~a" orgelidx target faderidx value)
    (setf (aref (slot-value (aref *curr-state* orgelidx) target) f-idx) value)
    (orgel-ctl-fader (orgel-name (1+ orgelidx)) target faderidx value)
    (mapcar #'funcall (aref
                       (slot-value (aref *osc-responder-registry* orgelidx) target)
                       f-idx))))

;;; (orgel-fader-value-callback 0 'level 3 1 nil)

(defun orgel-mlevel-value-callback (orgelidx faderidx value src)
  (declare (ignore src))
  (let ((f-idx (round (1- faderidx))))
    (setf (aref (aref *orgel-mlevel* orgelidx) f-idx) value)
    (orgel-ctl-fader (orgel-name (1+ orgelidx)) :mlevel faderidx value)
    (mapcar #'funcall (aref
                       (slot-value (aref *osc-responder-registry* orgelidx) :mlevel)
                       f-idx))))
