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

(defparameter *orgel-global-targets*
  '(:base-freq :phase :main :min-amp :max-amp :ramp-up :ramp-down :exp-base :bias))

(defparameter *orgel-fader-targets*
  '(:level :delay :q :gain :osc-level))

(defparameter *orgel-measure-targets*
  '(:mlevel))

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
  (level (make-array 16 :initial-element 0.0) :type simple-array)
  (delay (make-array 16 :initial-element 0.0) :type simple-array)
  (q (make-array 16 :initial-element 0.0) :type simple-array)
  (gain (make-array 16 :initial-element 0.0) :type simple-array)
  (osc-level (make-array 16 :initial-element 0.0) :type simple-array))

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
  "retrieve function object by keyword of its name."
  (symbol-function (intern (string-upcase (format nil "orgel-~a" target)))))

(defun orgel-slot-name (target)
  "convert keyword to symbol"
  (read-from-string (format nil "orgel-~a" target)))

;;; utility shorthand fns

(defmacro define-orgel-fader-access-fn (target)
  `(defun ,(intern (string-upcase (format nil "~a" target))) (orgelnummer idx)
     (aref (,(intern (string-upcase (format nil "orgel-~a" target))) (aref *curr-state* (1- orgelnummer))) (1- idx))))

(define-orgel-fader-access-fn :level)
(define-orgel-fader-access-fn :gain)
(define-orgel-fader-access-fn :delay)
(define-orgel-fader-access-fn :q)
(define-orgel-fader-access-fn :osc-level)

(setf (fdefinition 'ophase) #'orgel-phase)
(setf (fdefinition 'min-amp) #'orgel-min-amp)
(setf (fdefinition 'max-amp) #'orgel-max-amp)
(setf (fdefinition 'base-freq) #'orgel-base-freq)
(setf (fdefinition 'ramp-up) #'orgel-ramp-up)
(setf (fdefinition 'ramp-down) #'orgel-ramp-down)
(setf (fdefinition 'exp-base) #'orgel-exp-base)
(setf (fdefinition 'main) #'orgel-main)

(defun mlevel (orgelnummer idx)
  (aref (aref *orgel-mlevel* (1- orgelnummer)) (1- idx)))

(defstruct orgel-registry
  base-freq
  phase
  bias
  main
  min-amp
  max-amp
  ramp-up
  ramp-down
  exp-base
  (level (make-array 16 :initial-element nil) :type simple-array)
  (delay (make-array 16 :initial-element nil) :type simple-array)
  (q (make-array 16 :initial-element nil) :type simple-array)
  (gain (make-array 16 :initial-element nil) :type simple-array)
  (osc-level (make-array 16 :initial-element nil) :type simple-array)
  (mlevel (make-array 16 :initial-element nil) :type simple-array))
