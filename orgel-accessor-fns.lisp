;;; 
;;; orgel-accessor-fns.lisp
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

(defun orgel-access-fn (target)
  "retrieve function object by keyword of its name."
  (symbol-function (intern (string-upcase (format nil "orgel-~a" target)) :cl-orgelctl)))

(defun orgel-slot-name (target)
  "convert keyword to symbol"
  (read-from-string (format nil "orgel-~a" target)))

;;; utility shorthand access fns for the organ slots in *curr-state*


(defmacro define-orgel-fader-access-fn (target)
  `(defun ,(intern (string-upcase (format nil "~a" target))) (orgelnummer idx)
     ,(format nil "access function for the ~a slot with index <idx> of orgel at <orgelnummer> in *curr-state*." target)
     (aref (,(intern (string-upcase (format nil "orgel-~a" target)) :cl-orgelctl) (aref *curr-state* (1- orgelnummer))) (1- idx))))

(defmacro define-all-orgel-fader-access-fns (targets)
  `(progn
     ,@(loop
         for target in (eval targets)
         collect (list 'define-orgel-fader-access-fn target))))

(define-all-orgel-fader-access-fns *orgel-fader-targets*)

;; (define-orgel-fader-access-fn :level)
;; (define-orgel-fader-access-fn :gain)
;; (define-orgel-fader-access-fn :delay)
;; (define-orgel-fader-access-fn :q)
;; (define-orgel-fader-access-fn :osc-level)

(defmacro define-orgel-global-access-fn (target)
  `(defun ,(intern (string-upcase (format nil "~a" target))) (orgelnummer)
     ,(format nil "access function for the ~a slot of orgel at <orgelnummer> in *curr-state*." target)
     (,(intern (string-upcase (format nil "orgel-~a" target)) :cl-orgelctl) (aref *curr-state* (1- orgelnummer)))))

(defmacro define-all-orgel-global-access-fns (targets)
  `(progn
     ,@(loop
         for target in (eval targets)
         collect (list 'define-orgel-global-access-fn target))))

;;; for the following target list we need to remove phase as this
;;; symbol is used and we call the access function "ophase":

(define-all-orgel-global-access-fns (remove :phase *orgel-global-targets*))

(defun ophase (orgelnummer)
  (orgel-phase (aref *curr-state* (1- orgelnummer))))

;;; another special case, as mlevel isn't part of a preset and
;;; therefor not stored in *curr-state*

(defun mlevel (orgelnummer idx)
  (aref (aref *orgel-mlevel* (1- orgelnummer)) (1- idx)))
