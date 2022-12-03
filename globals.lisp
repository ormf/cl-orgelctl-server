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

(defparameter *num-orgel* 6) ;;; total num of organs

;;; the current state of all orgel vars:

(defvar *curr-state*
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
