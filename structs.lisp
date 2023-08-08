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
#|
(defstruct orgel
  (base-freq 0.0 :type float)
  (phase 0.0 :type float)
  (bias-pos 0.0 :type float)
  (bias-bw 0.0 :type float)
  (bias-type 0.0 :type float)
  (main 0.0 :type float)
  (min-amp 0.0 :type float)
  (max-amp 0.0 :type float)
  (ramp-up 0.0 :type float)
  (ramp-down 0.0 :type float)
  (exp-base 0.0 :type float)
  (level (make-array 16 :initial-element 0.0) :type simple-array)
  (bias-level (make-array 16 :initial-element 0.0) :type simple-array)
  (delay (make-array 16 :initial-element 0.0) :type simple-array)
  (q (make-array 16 :initial-element 0.0) :type simple-array)
  (gain (make-array 16 :initial-element 0.0) :type simple-array)
  (osc-level (make-array 16 :initial-element 0.0) :type simple-array))
|#

(defun copy-orgel (original)
  (make-orgel
   :base-freq (orgel-base-freq original)
   :phase (orgel-phase original)
   :bias-pos (orgel-bias-pos original)
   :bias-bw (orgel-bias-bw original)
   :bias-type (orgel-bias-type original)
   :main (orgel-main original)
   :min-amp (orgel-min-amp original)
   :max-amp (orgel-max-amp original)
   :ramp-up (orgel-ramp-up original)
   :ramp-down (orgel-ramp-down original)
   :exp-base (orgel-exp-base original)
   :level (copy-seq (orgel-level original))
   :bias-level (copy-seq (orgel-bias-level original))
   :delay (copy-seq (orgel-delay original))
   :q (copy-seq (orgel-q original))
   :gain (copy-seq (orgel-gain original))
   :osc-level (copy-seq (orgel-osc-level original))))


(defclass orgel-registry ()
  ((base-freq :initform nil)
   (phase :initform nil)
   (bias-pos :initform nil)
   (bias-bw :initform nil)
   (bias-type :initform nil)
   (main :initform nil)
   (min-amp :initform nil)
   (max-amp :initform nil)
   (ramp-up :initform nil)
   (ramp-down :initform nil)
   (exp-base :initform nil)
   (level :initform (make-array 16 :initial-element nil) :type simple-array)
   (bias-level :initform (make-array 16 :initial-element nil) :type simple-array)
   (delay :initform (make-array 16 :initial-element nil) :type simple-array)
   (q :initform (make-array 16 :initial-element nil) :type simple-array)
   (gain :initform (make-array 16 :initial-element nil) :type simple-array)
   (osc-level :initform (make-array 16 :initial-element nil) :type simple-array)
   (mlevel :initform (make-array 16 :initial-element nil) :type simple-array)))
