;;; 
;;; osc.lisp
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

(defvar *oscout* (incudine.osc:open :port 3010 :direction :output))
(defvar *oscin* (incudine.osc:open :port 3011 :host "127.0.0.1" :direction :input))
(defvar *orgel-osc-responder* (make-hash-table)) ;;; a hashtable with the handles of all orgel responders

;;; registry for functions to be called on incoming osc messages

(defparameter *osc-responder-registry*
  (make-array *num-orgel*
              :initial-contents (loop
                                  for i below *num-orgel*
                                  collect (make-orgel-registry))))
#|
(loop for target in *orgel-global-targets*
collect `(setf (,(read-from-string (format nil "orgel-registry-~a" target)) (aref *orgel-responder-registry* orgelidx)) nil))

|#

;;; The funcalls are spelled out for speed. Too lazy to do it with a macro...

(defun clear-osc-responder-registry ()
  "clear all function calls to be called on osc message receive."
  (loop
    for orgelidx below *num-orgel*
    do (progn
         (setf (orgel-registry-base-freq (aref *osc-responder-registry* orgelidx)) nil)
         (setf (orgel-registry-phase (aref *osc-responder-registry* orgelidx)) nil)
         (setf (orgel-registry-main (aref *osc-responder-registry* orgelidx)) nil)
         (setf (orgel-registry-min-amp (aref *osc-responder-registry* orgelidx)) nil)
         (setf (orgel-registry-max-amp (aref *osc-responder-registry* orgelidx)) nil)
         (setf (orgel-registry-ramp-up (aref *osc-responder-registry* orgelidx)) nil)
         (setf (orgel-registry-ramp-down (aref *osc-responder-registry* orgelidx)) nil)
         (setf (orgel-registry-exp-base (aref *osc-responder-registry* orgelidx)) nil)
         (setf (orgel-registry-bias (aref *osc-responder-registry* orgelidx)) nil)
         (setf (orgel-registry-base-freq (aref *osc-responder-registry* orgelidx)) nil)
         (dotimes (idx 16)
           (setf (aref (orgel-registry-level (aref *osc-responder-registry* orgelidx)) idx) nil))
         (dotimes (idx 16)
           (setf (aref (orgel-registry-mlevel (aref *osc-responder-registry* orgelidx)) idx) nil))
         (dotimes (idx 16)
           (setf (aref (orgel-registry-delay (aref *osc-responder-registry* orgelidx)) idx) nil))
         (dotimes (idx 16)
           (setf (aref (orgel-registry-q (aref *osc-responder-registry* orgelidx)) idx) nil))
         (dotimes (idx 16)
           (setf (aref (orgel-registry-gain (aref *osc-responder-registry* orgelidx)) idx) nil))
         (dotimes (idx 16)
           (setf (aref (orgel-registry-osc-level (aref *osc-responder-registry* orgelidx)) idx) nil)))))


#|
(defmacro struct-accessor (struct-name slot instance)
  `(,(intern (string-upcase (format nil "~a-~a" struct-name slot))) ,instance))

;;; (struct-accessor :orgel :bias '*test*)

(defun struct-accessor (struct-name slot instance)
  (list (intern (string-upcase (format nil "~a-~a" struct-name slot))) instance))
|#

(let ((orgelidx 1))
  (mapcar #'funcall (orgel-registry-base-freq
                     (aref *osc-responder-registry* (1- orgelidx)))))


(defmacro define-orgel-fader-responder (stream orgelidx target)
  `(list ,target
         (incudine::make-osc-responder
          ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "ff"
          (lambda (i f)
            (setf (aref (,(orgel-slot-name (symbol-value target)) (aref *curr-state* ,orgelidx)) (round (1- i))) f)
            (mapcar #'funcall (aref
                               (,(read-from-string (format nil "orgel-registry-~a" (symbol-value target)))
                                (aref *osc-responder-registry* ,orgelidx))
                               (round (1- i))))
            (if *debug* (format t "orgel~2,'0d: ~a ~a ~a~%" ,(1+ orgelidx) ,target (round i) f))))))

(defmacro get-orgel-fader-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-orgel-fader-responder ,stream ,orgelidx ,target))))

(defmacro define-orgel-global-responder (stream orgelidx target)
  `(list ,target
         (incudine::make-osc-responder
          ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "ff"
          (lambda (f)
            (setf (,(orgel-slot-name (symbol-value target)) (aref *curr-state* ,orgelidx)) f)
            (mapcar #'funcall (,(read-from-string (format nil "orgel-registry-~a" (symbol-value target)))
                               (aref *osc-responder-registry* ,orgelidx)))
            (if *debug* (format t "orgel~2,'0d: ~a ~a~%" ,(1+ orgelidx) ,target f))))))

(defmacro get-orgel-global-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-orgel-global-responder ,stream ,orgelidx ,target))))

(defmacro define-orgel-measure-responder (stream orgelidx target)
  `(list ,target
         (incudine::make-osc-responder
          ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "ff"
          (lambda (i f)
            (setf (aref (aref *orgel-mlevel* ,orgelidx) (round (1- i))) f)
            (mapcar #'funcall (aref
                               (,(read-from-string (format nil "orgel-registry-~a" (symbol-value target)))
                                (aref *osc-responder-registry* ,orgelidx))
                               (round (1- i))))
            (if *debug* (format t "orgel~2,'0d: ~a ~a ~a~%" ,(1+ orgelidx) , target (round i) f))))))

(defmacro get-orgel-measure-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-orgel-measure-responder ,stream ,orgelidx ,target))))

(defmacro make-all-responders (maxorgel stream)
  (let ((maxorgel (eval maxorgel)))
    `(progn
       ,@(loop
           for orgelidx below maxorgel
           collect `(setf (gethash ,(ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)
                          (append
                           (get-orgel-fader-responders ,stream ,orgelidx *orgel-fader-targets*)
                           (get-orgel-global-responders ,stream ,orgelidx *orgel-global-targets*)
                           (get-orgel-measure-responders ,stream ,orgelidx *orgel-measure-targets*)))))))



#|
(defun make-responders (orgelidx &optional (stream *oscin*))
  (dolist (target *orgel-fader-targets*)
    (let* ((target target))
      (setf (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)
            (let ((fn (orgel-access-fn target)))
              (append (list target
                            (incudine::make-osc-responder
                             stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "ff"
                             (lambda (i f)
                               (setf (aref (funcall fn (aref *curr-state* orgelidx)) (round (1- i))) f)
                               (if *debug* (format t "orgel~2,'0d: ~a ~a ~a~%" (1+ orgelidx) target (round i) f)))))
                      (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*))))))
  (dolist (target *orgel-global-targets*)
    (setf (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)
          (append (list target
                        (let ((slot (orgel-slot-name target)))
                          (incudine::make-osc-responder
                           stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "f"
                           (lambda (f)
                             (setf (slot-value (aref *curr-state* orgelidx) slot) f)
                             (if *debug* (format t "orgel~2,'0d: ~a ~a~%" (1+ orgelidx) target f))))))
                  (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*))))
  (dolist (target *orgel-measure-targets*)
    (let* ((target target))
      (setf (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)
            (let (;; (fn (orgel-access-fn target))
                  )
              (append (list target
                            (incudine::make-osc-responder
                             stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "ff"
                             (lambda (i f)
                               (setf (aref (aref *orgel-mlevel* orgelidx) (round (1- i))) f)
                               (if *debug* (format t "orgel~2,'0d: ~a ~a ~a~%" (1+ orgelidx) target (round i) f)))))
                      (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)))))))
|#

;;; (incudine.osc:close *oscout*)
;;; (incudine.osc:close *oscin*)

(defun orgel-ctl (orgelnum target idx val)
  (incudine.osc:message *oscout* (format nil "/orgel~2,'0d/~a" orgelnum target) "if" (round idx) (float val 1.0)))

(defun orgel-ctl-global (orgelnum target val)
  (incudine.osc:message *oscout* (format nil "/orgel~2,'0d/~a" orgelnum target) "f" (float val 1.0)))
