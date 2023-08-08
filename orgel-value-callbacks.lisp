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
  (setf (slot-value (aref *curr-state* orgelidx) target) value)
  (orgel-ctl (orgel-name (1+ orgelidx)) target value)
  (let ((accessor (cl-orgel-gui::slot->function "g-orgel" target))
        (val-string (cl-orgel-gui::ensure-string (if (member target '(bias-pos bias-bw main)) (* 100 value) value)))
        (attribute (member target '(bias-type phase))))
    (maphash (lambda (connection-id connection-hash)
               (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
               (let* ((orgel-gui (gethash "orgel-gui" connection-hash))
                      (orgel (aref (cl-orgel-gui::orgel-gui-orgeln orgel-gui) orgelidx)))
                   (when orgel-gui (let ((elem (funcall accessor orgel)))
;;;                                     (break "~a ~a ~S" elem (clog::value elem) val-string)
                                     (unless (equal src elem)
                                       (if attribute
                                           (setf (clog::attribute elem "data-val") val-string)
                                           (setf (clog::value elem) val-string)))))))
               clog-connection::*connection-data*))
  (mapcar #'funcall (slot-value (aref *osc-responder-registry* orgelidx) target)))

;;; (orgel-ctl (orgel-name 1) :base-freq 28)


(defun orgel-fader-value-callback (orgelidx target faderidx value src)
  (let ((f-idx (round (1- faderidx))))
;;;    (break "orgelidx: ~a, target: ~a ,faderidx: ~a, value: ~a" orgelidx target faderidx value)
    (setf (aref (slot-value (aref *curr-state* orgelidx) target) f-idx) value)
    (orgel-ctl-fader (orgel-name (1+ orgelidx)) target faderidx value)
    (let ((accessor (cl-orgel-gui::slot->function "g-orgel" target))
          (val-string (cl-orgel-gui::ensure-string (* 100 value))))
      (maphash (lambda (connection-id connection-hash)
                 (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                 (let* ((orgel-gui (gethash "orgel-gui" connection-hash))
                        (orgel (aref (cl-orgel-gui::orgel-gui-orgeln orgel-gui) orgelidx)))
                   (when orgel-gui (let ((elem (aref (funcall accessor orgel) f-idx)))
;;;                                     (break "~a ~a ~S" elem (clog::value elem) val-string)
                                     (unless (equal src elem) (setf (clog::value elem) val-string))))))
               clog-connection::*connection-data*))
    (mapcar #'funcall (aref
                       (slot-value (aref *osc-responder-registry* orgelidx) target)
                       f-idx))))

;;; (orgel-fader-value-callback 0 'level 3 0.7 nil)

(defun orgel-mlevel-value-callback (orgelidx faderidx value src)
  (let ((f-idx (round (1- faderidx))))
;;;    (format t "mlevel2-value-callback: ~a ~a ~a~%" orgelidx faderidx value)
    (setf (aref (aref *orgel-mlevel* orgelidx) f-idx) value)
;;;    (orgel-ctl-fader (orgel-name (1+ orgelidx)) :mlevel faderidx value)
    (let ((accessor (cl-orgel-gui::slot->function "g-orgel" 'meters))
          (val-string (cl-orgel-gui::ensure-string (- value 100))))
        (maphash (lambda (connection-id connection-hash)
                   (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                   (let* ((orgel-gui (gethash "orgel-gui" connection-hash))
                          (orgel (aref (cl-orgel-gui::orgel-gui-orgeln orgel-gui) orgelidx)))
                     (when orgel-gui
;;;                       (format t "~a" (funcall accessor orgel))
                       (let ((elem (elt (funcall accessor orgel) (round (1- faderidx)))))
;;;                                       (break "self: ~a~% elem: ~a" self elem)
                                       (unless (equal src elem) (setf (clog::attribute elem "data-db") val-string))))))
                 clog-connection::*connection-data*))
    (mapcar #'funcall (aref
                       (slot-value (aref *osc-responder-registry* orgelidx) 'mlevel)
                       f-idx))))


