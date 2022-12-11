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

(defun recall-orgel (orgel num &optional next interp)
  (dolist (slot '(:level :delay :q :gain :osc-level))
    (dotimes (i 16)
      (let ((val (if next
                     (+
                      (* (if interp (- 1 interp) 0.5)
                         (aref (funcall (orgel-access-fn slot) (aref (aref *orgel-presets* num) orgel)) i))
                      (* (or interp 0.5)
                         (aref (funcall (orgel-access-fn slot) (aref (aref *orgel-presets* next) orgel)) i)))
                     (aref (funcall (orgel-access-fn slot) (aref (aref *orgel-presets* num) orgel)) i))))
        (if *debug* (format t "sending: orgel~2,'0d: ~a ~a ~a~%" (1+ orgel) slot (1+ i) val))
        (orgel-ctl (1+ orgel) slot (1+ i) val))))
  (dolist (slot *orgel-global-targets*)
    (let ((val (if next
                    (+
                     (* (if interp (- 1 interp) 0.5)
                        (funcall (orgel-access-fn slot) (aref (aref *orgel-presets* num) orgel)))
                     (* (or interp 0.5)
                        (funcall (orgel-access-fn slot) (aref (aref *orgel-presets* next) orgel))))
                    (funcall (orgel-access-fn slot) (aref (aref *orgel-presets* num) orgel)))))
      (if *debug* (format t "sending: orgel~2,'0d: ~a ~a~%" (1+ orgel) slot val))
      (orgel-ctl-global (1+ orgel) slot val))))

(defun recall-preset (num &optional next interp)
  (loop for orgel below *num-orgel*
        for time from 0 by 0.005
        do (let ((orgel orgel))
             (cm:at (+ (cm:now) time) (lambda () (recall-orgel orgel num next interp))))))

;;; (recall-preset 1)

(defun save-presets (&optional (file "/tmp/orgel-presets.lisp"))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (format out "(setf *orgel-presets* ~%~a)" *orgel-presets*)))

;;; (save-presets)

(defun load-presets (file)
  (load file))

;;; (orgel-ctl 1 :level 1 (random 128))


(defparameter *route-presets* (make-array 128 :initial-element nil :element-type 'list))

(defun digest-route-preset (preset-num form)
  (setf (aref *route-presets* preset-num) form)
  (recall-preset (getf form :preset))
  (digest-routes (getf form :routes)))

(defun save-route-presets (&optional (file "./presets/route-presets.lisp"))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (format out "(in-package :cl-orgelctl)~%~%(setf *route-presets*~%~a)" *route-presets*)))

(defun recall-route-preset (num)
  (let ((form (aref *route-presets* num)))
    (recall-preset (getf form :preset))
    (digest-routes (getf form :routes))
))

;;; (recall-route-preset 0)
;;; (save-route-presets)

