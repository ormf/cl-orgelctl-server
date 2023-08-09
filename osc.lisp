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
  (make-array *orgelcount*
              :initial-contents (loop
                                  for i below *orgelcount*
                                  collect (make-instance 'orgel-registry))))
#|
(loop for target in *orgel-global-targets*
collect `(setf (,(read-from-string (format nil "orgel-registry-~a" target)) (aref *orgel-responder-registry* orgelidx)) nil))

|#

;;; The funcalls are spelled out for speed. Too lazy to do it with a macro...

(defun clear-osc-responder-registry ()
  "clear all function calls to be called on osc message receive."
  (loop
    for orgelidx below *orgelcount*
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
  "responder for the fader controllers of the 16 partials (level, delay,
bp, gain, osc-level)."
  `(list ,target
         (incudine::make-osc-responder
          ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "ff"
          (lambda (faderidx value)
            (orgel-fader-value-callback ,orgelidx ',(read-from-string (format nil "~a" (symbol-value target))) faderidx value nil)
            (if *debug* (format t "orgel~2,'0d: ~a ~a ~a~%" ,(1+ orgelidx) ,target (round faderidx) value))))))

(defmacro get-orgel-fader-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-orgel-fader-responder ,stream ,orgelidx ,target))))

(defmacro define-orgel-global-responder (stream orgelidx target)
  "responder for the global parameters of the organ (ramps, base-freq,
amps, etc.)"
  `(list ,target
         (incudine::make-osc-responder
          ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "f"
          (lambda (value)
            (orgel-global-value-callback ,orgelidx ',(read-from-string (format nil "~a" (symbol-value target))) value nil)
            (if *debug* (format t "orgel~2,'0d: ~a ~a~%" ,(1+ orgelidx) ,target value))))))

;;; (define-orgel-global-responder 'osc-stream 0 :base-freq)

(defmacro get-orgel-global-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-orgel-global-responder ,stream ,orgelidx ,target))))

(defmacro define-orgel-measure-responder (stream orgelidx target)
  "responder for the 16 output level meters."
  `(list ,target
         (incudine::make-osc-responder
          ,stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) (symbol-value target)) "ff"
          (lambda (faderidx value)
            (orgel-mlevel-value-callback ,orgelidx faderidx value nil)
            (if *debug* (format t "orgel~2,'0d: ~a ~a ~a~%" ,(1+ orgelidx) ,target (round faderidx) value))))))

(defmacro get-orgel-measure-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-orgel-measure-responder ,stream ,orgelidx ,target))))

;;; (define-orgel-measure-responder *oscin* 0 :mlevel)

(defun define-preset-responder (stream path fn)
  `(incudine:make-osc-responder
    ,stream
    ,(format nil "/preset-ctl/~a" path)
    ""
    (lambda ()
      ,fn
      (if *debug* (format t "preset-ctl: ~a~%" ,path)))))

(defmacro get-preset-responders (stream)
  `(progn
     ,(define-preset-responder stream "prev-preset" '(previous-orgel-preset))
     ,(define-preset-responder stream "next-preset" '(next-orgel-preset))
     ,(define-preset-responder stream "recall-preset" '(recall-orgel-preset *curr-orgel-preset-nr*))
     ,(define-preset-responder stream "store-preset" '(store-orgel-preset *curr-orgel-preset-nr*))
     ,(define-preset-responder stream "load-presets" '(load-orgel-presets))
     ,(define-preset-responder stream "save-presets" '(save-orgel-presets))
     (incudine:make-osc-responder
      ,stream
      "/preset-ctl/preset-no" "f"
      (lambda (f)
        (setf *curr-orgel-preset-nr* (round f))
        (if *debug* (format t "preset-ctl: preset-no ~a~%" (round f)))))))

#|
(defmacro get-preset-responders (stream)
  `(progn
     (incudine:make-osc-responder
      ,stream
      "/preset-ctl/prev-preset" ""
      (lambda ()
        (previous-orgel-preset)
        (if *debug*
            (format t "preset-ctl: prev-preset~%"))))
     (incudine:make-osc-responder
      ,stream
      "/preset-ctl/next-preset" ""
      (lambda ()
        (next-orgel-preset)
        (if *debug*
            (format t "preset-ctl: next-preset~%"))))
     (incudine:make-osc-responder
      ,stream
      "/preset-ctl/recall-preset" ""
      (lambda ()
        (recall-orgel-preset *curr-orgel-preset-nr*)
        (if *debug*
            (format t "preset-ctl: recall-preset~%"))))
     (incudine:make-osc-responder
      ,stream
      "/preset-ctl/store-preset" ""
      (lambda ()
        (store-orgel-preset *curr-orgel-preset-nr*)
        (if *debug*
            (format t "preset-ctl: store-preset~%"))))
     (incudine:make-osc-responder
      ,stream
      "/preset-ctl/load-presets" ""
      (lambda ()
        (load-orgel-presets)
        (if *debug*
            (format t "preset-ctl: load-presets~%"))))
     (incudine:make-osc-responder
      ,stream
      "/preset-ctl/save-presets" ""
      (lambda ()
        (save-orgel-presets)
        (if *debug*
            (format t "preset-ctl: save-presets~%"))))
     (incudine:make-osc-responder
      ,stream
      "/preset-ctl/preset-no" "f"
      (lambda (f)
        (setf *curr-orgel-preset-nr* (round f))
        (if *debug*
(format t "preset-ctl: preset-no ~a~%" (round f)))))))
|#

(defmacro make-all-responders (maxorgel stream)
  (let ((maxorgel (eval maxorgel)))
    `(progn
       (get-preset-responders ,stream)
       ,@(loop
           for orgelidx below maxorgel
           collect `(setf (gethash ,(ou:make-keyword (format nil "orgel~2,'0d" (1+ orgelidx))) *orgel-osc-responder*)
                          (append
                           (get-orgel-fader-responders ,stream ,orgelidx *orgel-fader-targets*)
                           (get-orgel-global-responders ,stream ,orgelidx *orgel-global-targets*)
                           (get-orgel-measure-responders ,stream ,orgelidx *orgel-measure-targets*))))
       nil)))

;;; call this in the init file: (make-all-responders *orgelcount* *oscin*)

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
;;;  (break "orgel: ~a ~a ~a ~a" orgel target idx val)
  (incudine.osc:message *oscout* (format nil "/~a/~a" orgel target) "if" (round idx) (float val 1.0)))

(declaim (inline target-key))
(defun target-key (target)
  (if (keywordp target) target
      (format nil "orgel~2,'0d" target)))

(defun orgel-ctl (orgeltarget target val)
  (let ((form (cond ((listp target) target)
                    ((keywordp target) (gethash target *observed*))
                    (t (list target))))
        (orgeltarget (target-key orgeltarget)))
    (if *debug* (format t (format nil "/~a/~a" orgeltarget (first form))))
    (unless form (error "target ~S doesn't exist" target))
    (if (cdr form)
        (incudine.osc:message *oscout* (format nil "/~a/~a" orgeltarget (first form)) "if"
                              (second form) (float val 1.0))
        (incudine.osc:message *oscout* (format nil "/~a/~a" orgeltarget (first form)) "f" (float val 1.0)))))

