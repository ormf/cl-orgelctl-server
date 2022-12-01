;;; 
;;; structs.lisp
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

(defstruct orgel
  (base-freq 0.0 :type float)
  (phase 0.0 :type float)
  (bias 0.0 :type float)
  (main 0.0 :type float)
  (min-amp 0.0 :type float)
  (max-amp 0.0 :type float)
  (ramp-up 0.0 :type float)
  (ramp-down 0.0 :type float)
  (exp-base 0.0 :type float)
  (level (make-array 16) :type simple-array)
  (delay (make-array 16) :type simple-array)
  (q (make-array 16) :type simple-array)
  (gain (make-array 16) :type simple-array)
  (osc-level (make-array 16) :type simple-array))

(defun copy-orgel (original)
  (make-orgel
   :base-freq (orgel-base-freq original)
   :phase (orgel-phase original)
   :bias (orgel-bias original)
   :main (orgel-main original)
   :min-amp (orgel-min-amp original)
   :max-amp (orgel-max-amp original)
   :ramp-up (orgel-ramp-up original)
   :ramp-down (orgel-ramp-down original)
   :exp-base (orgel-exp-base original)
   :level (copy-seq (orgel-level original))
   :delay (copy-seq (orgel-delay original))
   :q (copy-seq (orgel-q original))
   :gain (copy-seq (orgel-gain original))
   :osc-level (copy-seq (orgel-osc-level original))))

(defun orgel-access-fn (target)
  (symbol-function (intern (string-upcase (format nil "orgel-~a" target)))))

(defun orgel-slot-name (target)
  (intern (string-upcase (format nil "~a" target))))


;;; (gethash :level *orgel-slots*)

(defvar *curr-state*
  (make-array
   5
   :element-type 'orgel
   :initial-contents (loop for i below 5 collect (make-orgel))))
