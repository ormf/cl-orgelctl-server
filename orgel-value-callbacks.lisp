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
  (set-cell (slot-value (aref *curr-state* orgelidx) target) value :src src)
  #|
  (orgel-ctl (orgel-name (1+ orgelidx)) target value)
  (let ((accessor (cl-orgel-gui::slot->function "g-orgel" target))
        (val-string (cl-orgel-gui::ensure-string (if (member target '(bias-pos bias-bw main)) (* 100 value) value)))
        (attribute (member target '(:bias-type :phase))))
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
  (mapcar #'funcall (slot-value (aref *osc-responder-registry* orgelidx) target))
  |#
  )

#|
  (orgel-ctl (orgel-name (1+ orgelidx)) target value)
  (let ((accessor (cl-orgel-gui::slot->function "g-orgel" target))
        (val-string (cl-orgel-gui::ensure-string (if (member target '(bias-pos bias-bw main)) (* 100 value) value)))
        (attribute (member target '(:bias-type :phase))))
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
  (mapcar #'funcall (slot-value (aref *osc-responder-registry* orgelidx) target))

|#
;;; (orgel-ctl (orgel-name 1) :base-freq 28)


(defun orgel-fader-value-callback (orgelidx target faderidx value src)
  (let ((f-idx (round (1- faderidx))))
;;;    (break "orgelidx: ~a, target: ~a ,faderidx: ~a, value: ~a" orgelidx target faderidx value)
    (set-cell (aref (slot-value (aref *curr-state* orgelidx) target) f-idx) value :src src)
    #|
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
                       f-idx))
    |#
    ))

;;; (orgel-fader-value-callback 0 'level 3 0.7 nil)

(defun orgel-mlevel-value-callback (orgelidx faderidx value src)
  (let ((f-idx (round (1- faderidx))))
;;;    (format t "mlevel2-value-callback: ~a ~a ~a~%" orgelidx faderidx value)
    (set-cell (aref (aref *orgel-mlevel* orgelidx) f-idx) value :src src)
    #|
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
                       f-idx))
    |#
    ))

;;; (setf (set-cell-hook (slot-value (aref *curr-state* 0) :base-freq)) (lambda (x &key src) src x))

(defun setup-ref-cell-hooks ()
  "Set up propagating changes in the model-slots of *curr-state* to all
connected clients."
  (dotimes (orgelidx *orgelcount*)
    (let ((global-orgel (aref *curr-state* orgelidx)))
      (map nil (lambda (slot-sym slot-key)
                 (let ((orgelidx orgelidx))
                   (setf (set-cell-hook (slot-value global-orgel slot-sym))
                         (lambda (val &key src)
                           (declare (ignorable src))
                           (format t "cell-hook! orgelidx: ~a~%" orgelidx)
                           (global-to-pd (orgel-name (1+ orgelidx)) slot-key val)
                           (let ((val-string (format nil "~,1f" (* val (if (member slot-key '(:main :bias-bw :bias-pos)) 100 1))))
                                 (attribute (if (member slot-key '(:bias-type :phase)) "data-val")))
                             (maphash (lambda (connection-id connection-hash)
                                        (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                                        (let* ((orgel-gui (gethash "orgel-gui" connection-hash)))
                                          (when orgel-gui (let ((elem (slot-value (aref (orgel-gui-orgeln orgel-gui) orgelidx)
                                                                                  slot-sym)))
                                                            (unless (equal src elem)
                                                              (if attribute
                                                                  (setf (clog:attribute elem attribute) val-string)
                                                                  (setf (clog:value elem) val-string)))))))
                                      clog-connection::*connection-data*))))))
           *orgel-global-target-syms*
           *orgel-global-targets*)

      (map nil (lambda (slot-sym slot-key)
                 (let ((orgelidx orgelidx))
                   (dotimes (faderidx 16)
                     (let ((faderidx faderidx))
                       (setf (set-cell-hook (aref (slot-value global-orgel slot-sym) faderidx))
                             (lambda (val &key src)
                               (declare (ignorable src))
                               (fader-to-pd (orgel-name (1+ orgelidx)) slot-key (1+ faderidx) val)
                               (let ((val-string (format nil "~,1f" (* val 100))))
                                 (maphash (lambda (connection-id connection-hash)
                                            (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                                            (let* ((orgel-gui (gethash "orgel-gui" connection-hash)))
                                              (when orgel-gui (let ((elem (aref (slot-value (aref (orgel-gui-orgeln orgel-gui) orgelidx) slot-sym)
                                                                                faderidx)))
                                                                (unless (equal src elem) (setf (clog:value elem) val-string))))))
                                          clog-connection::*connection-data*))))))))
            *orgel-fader-target-syms*
            *orgel-fader-targets*)

      (let* ((orgelidx orgelidx)
             (slot-sym 'meters)
             (slot-key :mlevel)
             (global-meter-ref (aref *orgel-mlevel* orgelidx)))
        (dotimes (faderidx 16)
          (let ((faderidx faderidx))
            (setf (set-cell-hook (aref global-meter-ref faderidx))
                  (lambda (val &key src)
                    (declare (ignorable src))
                    (fader-to-pd (orgel-name (1+ orgelidx)) slot-key (1+ faderidx) val)
                    (let ((meter-value (- (val (aref (aref *orgel-mlevel* orgelidx) faderidx)) 100)))
                      (maphash (lambda (connection-id connection-hash)
                                 (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                                 (let* ((orgel-gui (gethash "orgel-gui" connection-hash)))
                                   (when orgel-gui (let ((elem (aref (slot-value (aref (orgel-gui-orgeln orgel-gui) orgelidx) slot-sym)
                                                                     faderidx)))
                                                     (unless (equal src elem)
                                                       (setf (clog:attribute elem "data-db") meter-value))))))
                               clog-connection::*connection-data*)))))))
      *orgel-fader-target-syms*
      *orgel-fader-targets*)))


;;; (setup-ref-cell-hooks)
