;;; 
;;; presets.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

;;; *orgel-presets* currently just contains the static positions of
;;; all faders/numboxes, but no routing/relation of faders!

(defparameter *orgel-presets*
  (make-array
   128
   :initial-contents
   (loop
     for i below 128
     collect (make-array
              *num-orgel*
              :initial-contents
              (loop
                for x below *num-orgel*
                collect (make-orgel))))))

(defun copy-preset (src target)
  (dotimes (i *num-orgel*)
    (setf (aref target i)
          (copy-orgel (aref src i)))))

;;; (copy-preset *curr-state* (aref *orgel-presets* 0))
;;;(aref *curr-state* 1)

(defun recall-orgel (orgel num)
  (dolist (slot '(:level :delay :q :gain :osc-level))
    (dotimes (i 16)
      (if *debug* (format t "sending: orgel~2,'0d: ~a ~a ~a~%" (1+ orgel) slot (1+ i)
                          (aref
                           (funcall
                            (orgel-access-fn slot)
                            (aref (aref *orgel-presets* num) orgel))
                           i)))
      (orgel-ctl (1+ orgel) slot (1+ i)
                 (aref
                  (funcall
                   (orgel-access-fn slot)
                   (aref (aref *orgel-presets* num) orgel))
                  i))))
  (dolist (slot '(:base-freq :phase :bias :main :min-amp :max-amp :ramp-up :ramp-down :exp-base))
    (if *debug* (format t "sending: orgel~2,'0d: ~a ~a~%" (1+ orgel) slot
                        (funcall
                         (orgel-access-fn slot)
                         (aref (aref *orgel-presets* num) orgel))))
    (orgel-ctl-global (1+ orgel) slot
                      (funcall
                       (orgel-access-fn slot)
                       (aref (aref *orgel-presets* num) orgel)))))

(defun recall-preset (num)
  (loop for orgel below *num-orgel*
        for time from 0 by 0.005
        do (let ((orgel orgel))
             (cm:at (+ (cm:now) time) (lambda () (recall-orgel orgel num))))))

;;; (recall-preset 1)

(defun save-presets (&optional (file "/tmp/orgel-presets.lisp"))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (format out "(setf *orgel-presets* ~%~a)" *orgel-presets*)))

;;; (save-presets)

(defun load-presets (file)
  (load file))

;;; (orgel-ctl 1 :level 1 (random 128))


