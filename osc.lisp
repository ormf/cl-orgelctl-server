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

(defparameter *local-host* "127.0.0.1")
(defparameter *remote-host* "127.0.0.1")

(defparameter *oscout* (incudine.osc:open :port 3010 :host *remote-host* :direction :output :protocol :udp))
(defparameter *oscin* (incudine.osc:open :port 3011 :host *local-host* :direction :input :protocol :udp))
(defparameter *orgel-osc-responder* (make-hash-table)) ;;; a hashtable with the handles of all orgel responders


;;; (incudine.osc:close *oscin*)
;; (progn
;;   (incudine.osc:close *oscout*)
;;   (incudine.osc:close *oscin*))

;;; registry for functions to be called on incoming osc messages

(defparameter *osc-responder-registry*
  (make-array *num-orgel*
              :initial-contents (loop
                                  for i below *num-orgel*
                                  collect (make-instance 'orgel-registry))))
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
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'base-freq) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'phase) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'main) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'bias-pos) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'bias-bw) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'bias-type) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'min-amp) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'max-amp) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'ramp-up) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'ramp-down) nil)
         (setf (slot-value (aref *osc-responder-registry* orgelidx) 'exp-base) nil)
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'level) idx) nil))
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'bias-level) idx) nil))
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'mlevel) idx) nil))
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'delay) idx) nil))
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'q) idx) nil))
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'gain) idx) nil))
         (dotimes (idx 16)
           (setf (aref (slot-value (aref *osc-responder-registry* orgelidx) 'osc-level) idx) nil)))))


#|
(defmacro struct-accessor (struct-name slot instance)
  `(,(intern (string-upcase (format nil "~a-~a" struct-name slot))) ,instance))

;;; (struct-accessor :orgel :bias '*test*)

(defun struct-accessor (struct-name slot instance)
  (list (intern (string-upcase (format nil "~a-~a" struct-name slot))) instance))

(let ((orgelidx 1))
  (mapcar #'funcall (orgel-registry-base-freq
                     (aref *osc-responder-registry* (1- orgelidx)))))
|#

;;; registry of orgel-responder functions:

;;; We define generic responders for all incoming osc messages. They
;;; simply map over all functions of the respective entry of the
;;; incoming message in *osc-responder-registry*.
;;;
;;; To make this more efficient, macros are used to expand all the
;;; function definition code in order to reduce indirection/lookup
;;; overhead. On intialisation of the package, the macro
;;; make-all-responders should be called once with the number of
;;; organs to observe.
;;;
;;; Installing functions to be called boils down to pushing them to
;;; the lists of the respective entries in *osc-responder-registry*.


(defmacro define-orgel-fader-responder (stream orgelidx target)
  `(list ,target
         (incudine::make-osc-responder
          ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "ff"
          (lambda (i f)
            (setf (aref (,(orgel-slot-name (symbol-value target)) (aref *curr-state* ,orgelidx)) (round (1- i))) f)
            (mapcar #'funcall (aref
                               (slot-value (aref *osc-responder-registry* ,orgelidx)
                                          ',(read-from-string (format nil "~a" (symbol-value target))))
                               (round (1- i))))
            (if *debug* (format t "orgel~2,'0d: ~a ~a ~a~%" ,(1+ orgelidx) ,target (round i) f))))))

(defmacro get-orgel-fader-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-orgel-fader-responder ,stream ,orgelidx ,target))))

(defmacro define-orgel-global-responder (stream orgelidx target)
  `(list ,target
         (incudine::make-osc-responder
          ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "f"
          (lambda (f)
            (setf (,(orgel-slot-name (symbol-value target)) (aref *curr-state* ,orgelidx)) f)
            (mapcar #'funcall (slot-value (aref *osc-responder-registry* ,orgelidx)
                                          ',(read-from-string (format nil "~a" (symbol-value target)))))
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
                               (slot-value (aref *osc-responder-registry* ,orgelidx)
                                           ',(read-from-string (format nil "~a" (symbol-value target))))
                               (round (1- i))))
            (if *debug* (format t "orgel~2,'0d: ~a ~a ~a~%" ,(1+ orgelidx) , target (round i) f))))))

(defmacro get-orgel-measure-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-orgel-measure-responder ,stream ,orgelidx ,target))))

;;; (define-orgel-measure-responder *oscin* 0 :mlevel)

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

;;; call this in the init file: (make-all-responders *num-orgel* *oscin*)

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

(defun orgel-ctl-fader (orgel target idx val)
  (unless (gethash orgel *orgeltargets*) (error "Orgel \"~S\" doesn't exist" orgel))
  (incudine.osc:message *oscout* (format nil "/~a/~a" orgel target) "if" (round idx) (float val 1.0)))

(declaim (inline target-key))
(defun target-key (target)
  (if (keywordp target) target
      (format nil "orgel~2,'0d" target)))

(defun orgel-ctl (orgeltarget target val)
  (let ((form (if (listp target) target (gethash target *observed*)))
        (orgeltarget (target-key orgeltarget)))
    (unless form (error "target ~S doesn't exist" target))
    (if (cdr form)
        (incudine.osc:message *oscout* (format nil "/~a/~a" orgeltarget (first form)) "if" (second form) (float val 1.0))
        (incudine.osc:message *oscout* (format nil "/~a/~a" orgeltarget (first form)) "f" (float val 1.0)))))
