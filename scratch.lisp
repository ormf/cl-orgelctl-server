;;; 
;;; scratch.lisp
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

(copy-preset *curr-state* (aref *orgel-presets* 1))

(save-presets "/home/orm/work/unterricht/frankfurt/ws_22_23/musikinformatik/lisp/cl-orgelctl/orgel-presets.lisp")

(defparameter *test* (make-orgel))

(setf (orgel-bias *test*) 3.0)

(defun struct-accessor (struct-name slot instance)
  `(,(intern (string-upcase (format nil "~a-~a" struct-name slot))) ,instance))

(struct-accessor :orgel :bias '*test*)


(defun make-responders (orgelidx &optional (stream *oscin*))
  (dolist (target '(:level :delay :q :gain :osc-level))
    (let* ((target target))
      (setf (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)
            (let ((fn `(lambda (i f)
                         (setf (aref (funcall (gethash ,target *orgel-fns*) (aref *curr-state* ,orgelidx))
                                     (round (1- i)))
                               f)
                         (format t "orgel~2,'0d: ~a ~a ~a~%" (1+ orgelidx) ,target i f))))
              (append (list target
                            (incudine::make-osc-responder
                             stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "ff" fn))
                      (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*))))))
  (dolist (target '(:base-freq :phase :bias :main :min-amp :max-amp :ramp-up :ramp-down :exp-base))
    (setf (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)
          (append (list target
                        (incudine::make-osc-responder
                         stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "f"
                         (lambda (f)
                           (setf (slot-value (aref *curr-state* orgelidx) (gethash target *orgel-slots*)) f))))
                  (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)))))

(defun make-responders (orgelidx &optional (stream *oscin*))
  (dolist (target '(:level :delay :q :gain :osc-level))
    (let* ((target target))
      (setf (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)
            (append (list target
                        (incudine::make-osc-responder
                         stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "ff"
                         (lambda (i f)
                           (setf (aref (funcall (gethash target *orgel-fns*) target '(aref *curr-state* orgelidx))
                                       (round (1- i)))
                                 f)
                           (format t "orgel~2,'0d: ~a ~a ~a~%" (1+ orgelidx) target i f))))
                  (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)))))
  (dolist (target '(:base-freq :phase :bias :main :min-amp :max-amp :ramp-up :ramp-down :exp-base))
    (setf (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)
          (append (list target
                        (incudine::make-osc-responder
                         stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "f"
                         (lambda (f)
                           (setf (slot-value (aref *curr-state* orgelidx) (gethash target *orgel-slots*)) f))))
                  (gethash (ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)))))

(incudine::make-osc-responder
 stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "ff"
 (lambda (i f)
   (setf (aref (funcall (gethash target *orgel-fns*) target '(aref *curr-state* orgelidx))
               (round (1- i)))
         f)
   (format t "orgel~2,'0d: ~a ~a ~a~%" (1+ orgelidx) target i f)))

(let ((target :level))
  (incudine::make-osc-responder
   stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) :level) "ff"
   (lambda (i f)
     (setf (aref (orgel-level (aref *curr-state* orgelidx))
                 (round (1- i)))
           f)
     (format t "orgel~2,'0d: ~a ~a ~a~%" (1+ orgelidx) :level i f))))

()



(make-orgel-responder :level)

(defun struct-accessor (struct-name slot)
  (intern (string-upcase (format nil "~a-~a" struct-name slot))))

(struct-accessor 'orgel :level)

(incudine::make-osc-responder
 stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "f"
 (lambda (f)
   (setf (slot-value (aref *curr-state* orgelidx) (gethash target *orgel-slots*)) f)))


(defun orgel-automation (orgel target)
  (dotimes (idx 16)
    (orgel-ctl (1+ orgel) target (1+ idx) (random 128)))
  (cm::at (+ (cm::now) 0.01) #'orgel-automation (random 5)
          (r-elt '(:level :delay :q :gain)))
  )

(defun struct-accessor (struct-name slot instance)
  `(,(intern (string-upcase (format nil "~a-~a" struct-name slot))) ,instance))

(defmacro define-orgel-responder (orgelidx target)
  `(incudine::make-osc-responder
    stream (format nil "/orgel~2,'0d/~a" ,(1+ orgelidx) ,target) "ff"
    (lambda (i f)
      (setf (aref (,(struct-accessor :orgel target) `(aref *curr-state* ,orgelidx))
                  (round (1- i)))
            f)
      (format t "orgel~2,'0d: ~a ~a ~a~%" (1+ orgelidx) ,target i f))))

(let ((target :level))
  (define-orgel-responder 0 target))

(let ((fn '(lambda (x) x)))
  (incudine::make-osc-responder
   stream (format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "ff" fn))

(orgel-automation 0 :level)

(orgel-ctl-single 1 :base-freq 110.8)

(incudine.osc:message *oscout* (format nil "/orgel~2,'0d/~a" 1 :level) "ff" (float 3 1.0) (float 127 1.0))

(orgel-ctl 1 :delay 1 110.8)
*curr-state*

;;; (copy-preset *curr-state* (aref *orgel-presets* 0))

;;; (copy-orgel (aref *curr-state* 0))

(defparameter *test* (copy-orgel (aref *curr-state* 0)))
;;; (incudine.osc:close *oscout*)
;;; (incudine.osc:close *oscin*)

*curr-state*



(load)
(with-open-file "/tmp/orgel-presets.lisp")

(load "/tmp/orgel-presets.lisp")

(setf *orgel-presets* nil)

(aref *curr-state* 1)
