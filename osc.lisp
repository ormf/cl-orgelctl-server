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
(defvar *orgel-osc-responder* (make-hash-table))

#|
(defmacro struct-accessor (struct-name slot instance)
  `(,(intern (string-upcase (format nil "~a-~a" struct-name slot))) ,instance))

;;; (struct-accessor :orgel :bias '*test*)

(defun struct-accessor (struct-name slot instance)
  (list (intern (string-upcase (format nil "~a-~a" struct-name slot))) instance))
|#



(defun make-responders (orgelidx &optional (stream *oscin*))
  (dolist (target *orgel-fader-targets*)
    (let* ((target target))
      (setf (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)
            (let ((fn (orgel-access-fn target)))
              (append (list target
                            (incudine::make-osc-responder
                             stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "ff"
                             (lambda (i f)
                               (setf (aref (funcall fn (aref *curr-state* orgelidx))
                                           (round (1- i)))
                                     f)
                               (format t "orgel~2,'0d: ~a ~a ~a~%" (1+ orgelidx) target (round i) f))))
                      (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*))))))
  (dolist (target *orgel-single-targets*)
    (setf (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)
          (append (list target
                        (let ((slot (orgel-slot-name target)))
                          (incudine::make-osc-responder
                           stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "f"
                           (lambda (f)
                             (setf (slot-value (aref *curr-state* orgelidx) slot) f)
                             (format t "orgel~2,'0d: ~a ~a~%" (1+ orgelidx) target f)))))
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
                               ;; (setf (aref (funcall fn (aref *curr-state* orgelidx))
                               ;;             (round (1- i)))
                               ;;       f)
                               (format t "orgel~2,'0d: ~a ~a ~a~%" (1+ orgelidx) target (round i) f))))
                      (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)))))))

;;; (incudine.osc:close *oscout*)
;;; (incudine.osc:close *oscin*)

(defun orgel-ctl (orgelnum target idx val)
  (incudine.osc:message *oscout* (format nil "/orgel~2,'0d/~a" orgelnum target) "if" (round idx) (float val 1.0)))

(defun orgel-ctl-single (orgelnum target val)
  (incudine.osc:message *oscout* (format nil "/orgel~2,'0d/~a" orgelnum target) "f" (float val 1.0)))
