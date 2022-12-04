;;; 
;;; utils.lisp
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

(defun r-elt (seq)
  (elt seq (random (length seq))))

(defun reset-orgel-global ()
  (loop for orgel from 1 to 6 do
    (progn
      (orgel-ctl-global orgel :ramp-up 249)
      (orgel-ctl-global orgel :ramp-down 249)
      (orgel-ctl-global orgel :exp-base 0.3)
      (orgel-ctl-global orgel :min-amp 0)
      (orgel-ctl-global orgel :max-amp 1)
      (orgel-ctl-global orgel :phase 1)
      (orgel-ctl-global orgel :base-freq 117))))

;;; (reset-orgel-global)

(defun orgel-slot-fn (slot)
  "get the access function of slot keyword."
  (symbol-function (read-from-string (format nil "orgel-~a" slot))))

;;; (orgel-slot-fn :level)
