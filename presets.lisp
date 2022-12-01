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

(defvar *orgel-presets*
  (make-array
   128
   :initial-contents
   (loop
     for i below 128
     collect (make-array
              5
              :initial-contents
              (loop
                for x below 5
                collect (make-orgel))))))

(defun copy-preset (src target)
  (dotimes (i 5)
    (setf (aref target i)
          (copy-orgel (aref src i)))))

;;; (copy-preset *curr-state* (aref *orgel-presets* 0))
;;;(aref *curr-state* 1)

(defun recall-preset (num)
  (dotimes (orgel 5)
    (dolist (slot '(:level :delay :q :gain :osc-level))
      (dotimes (i 16)
        (orgel-ctl (1+ orgel) slot (1+ i)
                   (aref
                    (funcall
                     (gethash slot *orgel-fns*)
                     (aref (aref *orgel-presets* num) orgel))
                    i))))
    (dolist (slot '(:base-freq :phase :bias :main :min-amp :max-amp :ramp-up :ramp-down :exp-base))
      (orgel-ctl-single (1+ orgel) slot
                 (funcall
                  (gethash slot *orgel-fns*)
                  (aref (aref *orgel-presets* num) orgel))))))

;;; (recall-preset 1)

(defun save-presets (&optional (file "/tmp/orgel-presets.lisp"))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (format out "(setf *orgel-presets* ~%~a)" *orgel-presets*)))

;;; (save-presets)

(defun load-presets (file)
  (load file))

(defun r-elt (seq)
  (elt seq (random (length seq))))

;;; (orgel-ctl 1 :level 1 (random 128))


