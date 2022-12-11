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
    (setf (gethash target *orgel-preset-def-lookup*)
          `(set-faders ,target))
    (dotimes (i 16)
      (let ((fader-target (read-from-string (format nil ":~a~2,'0d" target (1+ i)))))
        (setf (gethash fader-target 
               *orgel-preset-def-lookup*)
              `(orgel-ctl ,fader-target)))))

  (dolist (target *orgel-global-targets*)
    (setf (gethash target *orgel-preset-def-lookup*)
          `(orgel-ctl ,target))))

(set-preset-def-lookup)

;;; the observed slots (accessor function names and keywords of all
;;; possible slots within the current orgel):

(defparameter *observed* (make-hash-table))

(progn
  (mapcar (lambda (x) (setf (gethash (read-from-string (format nil "~a" x)) *observed*) t))
          (append *orgel-global-targets* *orgel-fader-targets* *orgel-measure-targets* *midi-targets*))
  (mapcar (lambda (x) (loop
                   for slider from 1 to 16
                   do (setf (gethash
                             (read-from-string (format nil ":~a~2,'0d" x slider))
                             *observed*)
                            `(,(read-from-string (format nil "~a" x)) ,slider))))
          (append *orgel-fader-targets* *orgel-measure-targets*))
  (mapcar (lambda (x) (setf (gethash (read-from-string (format nil ":~a" x)) *observed*)
                       `(,(read-from-string (format nil "~a" x)))))
          *orgel-global-targets*))
        
(defun parse-observed (form)
  "extract all forms containing observed targets from form, replacing
keywords with their expanded access functions."
  (cond
    ((null form) nil)
    ((consp (first form))
     (append
      (if (gethash (caar form) *observed*)
          (list (first form))
          (parse-observed (car form)))
      (parse-observed (cdr form))))
    (:else (parse-observed (cdr form)))))

(defun replace-keywords (form orgelno)
  (cond
    ((null form) nil)
    ((keywordp (first form))
     (cons
      (let ((frm (gethash (first form) *observed*)))
        (if frm
            (list* (first frm) orgelno (rest frm))
            (first form)))
      (replace-keywords (cdr form) orgelno)))
    ((consp (first form))
     (cons
      (if (gethash (caar form) *observed*)
          (first form)
          (replace-keywords (first form) orgelno))
      (replace-keywords (cdr form) orgelno)))
    (:else (cons (first form)
                 (replace-keywords (rest form) orgelno)))))

(defun orgel-dependency-form (form)
  (if (gethash (first form) *observed*)
      (list form)))

;;; -> (base-freq phase main min-amp max-amp ramp-up ramp-down exp-base bias-pos level delay q gain osc-level mlevel)

(defun parse-observed (form)
  "extract all forms containing observed targets from form, replacing
keywords with their expanded access functions."
  (cond
    ((null form) nil)
    ((consp (first form))
     (append
      (or (orgel-dependency-form (first form))
          (parse-observed (car form)))
      (parse-observed (cdr form))))
    (:else (parse-observed (cdr form)))))

;;; (parse-observed '(+ (mlevel 1 1) :delay01 (- (level 1 1) (gain 2 1) (* 3 (base-freq 2)))) 1)
;;; -> ((mlevel 1 1) (delay 1 1) (level 1 1) (gain 2 1) (base-freq 2))

(defun get-fn (target orgel form)
  (let* ((call-spec (gethash target *orgel-preset-def-lookup*))
         (orgeltarget (if (member (first call-spec) '(set-faders))
                          orgel
                          orgel
;;;                          (get-orgel-no orgel)
                          )))
    (eval `(lambda () (,(first call-spec) ,orgeltarget
                  ,@(rest call-spec) ,form)))))

(defun register-responder (fn observed)
  (if (= (length observed) 3)
      (push fn (aref (slot-value (aref *osc-responder-registry* (1- (second observed))) (first observed))
                     (1- (third observed))))
      (push fn (slot-value (aref *osc-responder-registry* (1- (second observed))) (first observed)))))

(defun register-responders (target orgel form reset)
  (let* ((registered-fns '())
         (new-form (replace-keywords form (get-orgel-no orgel)))
         (fn (get-fn target orgel new-form)))
    (dolist (observed (parse-observed new-form))
      (register-responder fn observed)
      (when (and reset (not (member fn registered-fns)))
          (push fn registered-fns)
          (funcall fn)))))

(defun digest-route (orgel form reset)
  (loop
    with orgelno = (get-orgel-no orgel)
    for (target form) on form by #'cddr
    do (register-responders target orgel form reset)))

(defun digest-routes (form &key (reset t))
  (clear-osc-responder-registry)
  (loop
    for (orgel form) on form by #'cddr
    do (digest-route orgel form reset)))
