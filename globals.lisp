;;; 
;;; globals.lisp
;;;
;;; global variables are defined here.
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

(defparameter *debug* t)
(defparameter *num-orgel* 10) ;;; total num of organs

(defparameter *orgel-presets-file*
  (asdf:system-relative-pathname :cl-orgelctl "presets/orgel-presets.lisp"))

(defparameter *route-presets-file*
    (asdf:system-relative-pathname :cl-orgelctl "presets/route-presets.lisp"))

(defconstant +notch+ 1)
(defconstant +bandp+ 0)
(defconstant +phase+ 1)
(defconstant +invert+ -1)

;;; the current state of all orgel vars:

(defparameter *curr-state*
  (make-array
   *num-orgel*
   :element-type 'orgel
   :initial-contents (loop for i below *num-orgel* collect (make-orgel))))

(defparameter *orgel-mlevel*
  (make-array *num-orgel*
              :element-type 'simple-array
              :initial-contents
              (loop
                for i below *num-orgel*
                collect (make-array 16 :element-type 'float
                                       :initial-contents (loop for x below 16 collect 0.0)))))

(defparameter *orgeltargets* (make-hash-table))
(defparameter *global-targets* nil)
(defparameter *global-amps* nil)

(dotimes (i *num-orgel*)
  (setf (gethash (read-from-string (format nil ":orgel~2,'0d" (1+ i))) *orgeltargets*) i)
  (setf (gethash (1+ i) *orgeltargets*) i))

(defparameter *orgel-global-targets*
  '(:base-freq :phase :bias-pos :bias-bw :bias-type :main :min-amp :max-amp :ramp-up :ramp-down :exp-base))

(defparameter *orgel-fader-targets*
  '(:level :bias-level :delay :q :gain :osc-level :bias-level))

(defparameter *orgel-measure-targets*
  '(:mlevel))

(defparameter *midi-targets*
  '())

(defparameter *orgel-nr-lookup* nil)

(defparameter *orgel-name-lookup* nil)

(setf *orgel-nr-lookup*
      (loop for idx below *num-orgel*
            for name = (read-from-string (format nil ":orgel~2,'0d" (1+ idx)))
            append (list name (1+ idx))))

(setf *orgel-name-lookup*
      (coerce (cons nil (loop for (name idx) on *orgel-nr-lookup* by #'cddr collect name)) 'vector))
