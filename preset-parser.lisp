;;; 
;;; preset-parser.lisp
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

(defparameter *orgel-preset-def-lookup* (make-hash-table))

(defun set-preset-def-lookup ()
  (dolist (target *orgel-fader-targets*)
    (dotimes (i 16)
      (setf (gethash (read-from-string (format nil ":~a~2,'0d" target (1+ i)))
                     *orgel-preset-def-lookup*)
            `(orgel-ctl ,target ,(1+ i)))))

  (dolist (target *orgel-global-targets*)
    (setf (gethash target *orgel-preset-def-lookup*)
          `(orgel-ctl-global ,target))))

(set-preset-def-lookup)

;;; the observed slots (accessor function names):

(defparameter *observed*
  (mapcar (lambda (x) (read-from-string (format nil "~a" x)))
          (append *orgel-global-targets* *orgel-fader-targets* *orgel-measure-targets* *midi-targets*)))

;;; -> (base-freq phase main min-amp max-amp ramp-up ramp-down exp-base bias level delay q gain osc-level mlevel)

(defun parse-observed (form)
  "extract all forms containing observed targets from form."
  (cond
    ((null form) nil)
    ((consp (first form))
     (append
      (if (member (caar form) *observed*)
          (list (car form))
          (parse-observed (car form)))
      (parse-observed (cdr form))))
    (:else (parse-observed (cdr form)))))

;;; (parse-observed '(+ (mlevel 1 1) (- (level 1 1) (gain 2 1) (* 3 (base-freq 2)))))
;;; -> ((mlevel 1 1) (level 1 1) (gain 2 1) (base-freq 2))

(defun get-fn (target orgelno form)
  (let ((call-spec (gethash target *orgel-preset-def-lookup*)))
    (eval `(lambda () (,(first call-spec) ,orgelno
                  ,@(rest call-spec) ,form)))))

(defun register-responder (fn observed)
  (if (= (length observed) 3)
      (push fn (aref (slot-value (aref *osc-responder-registry* (1- (second observed))) (first observed))
                     (1- (third observed))))
      (push fn (slot-value (aref *osc-responder-registry* (1- (second observed))) (first observed)))))

(defun register-responders (target orgelno form)
  (let ((fn (get-fn target orgelno form)))
    (dolist (observed (parse-observed form))
      (register-responder fn observed))))

(defun digest-route (orgel form)
  (loop
    with orgelno = (get-orgel-no orgel)
    for (target form) on form by #'cddr
    do (register-responders target orgelno form)))

(defun digest-routes (form)
  (clear-osc-responder-registry)
  (loop
    for (orgel form) on form by #'cddr
    do (digest-route orgel form)))
