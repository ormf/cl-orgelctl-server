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


(make-pathname)

(save-presets "./presets/presets.lisp")

(load-presets "./presets/presets.lisp")

(defparameter *target-ranges* (make-hash-table))

(loop for (target vals) on
      '(:ramp-up (200 300.0)
        :ramp-down (200 300.0)
        :base-freq (100.0 900.0)
        :exp-base (0.3 0.8)
        :max-amp (1 1)
        :min-amp (0 0)
        :bias (1 16.0)
        :main (0.5 1.0))
      by #'cddr
      do (setf (gethash target *target-ranges*) vals))

(defun orgel-automation (orgel target)
  (let ((target (r-elt '(:bias :main :ramp-up :ramp-down :base-freq :exp-base :max-amp :min-amp))))
    (orgel-ctl-global (1+ (random 6)) target
                      (apply #'cm:between (gethash target *target-ranges*))))
  (dotimes (idx 16)
    (orgel-ctl (1+ orgel) target (1+ idx) (random 128)))
  (cm::at (+ (cm::now) 0.01) #'orgel-automation (random 6)
          (r-elt '(:level :delay :q :gain))))

(orgel-automation 0 :level)

(setf *debug* t)
(setf *debug* nil)
(copy-preset *curr-state* (aref *orgel-presets* 0))
(setf *debug* t)
(recall-preset 0)
(recall-preset 1)
(recall-preset 2)



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


(incudine:flush-pending)

*oscin*

(save-presets "/home/orm/work/unterricht/frankfurt/ws_22_23/musikinformatik/papierorgel/lisp/cl-orgelctl/presets/presets.lisp")

(incudine:flush-pending)
(incudine.osc:open)

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

(defun digest-preset)


(digest-preset
 :orgel-preset 1
 :orgel01
 (:level01 (+ (mlevel 1 1) (gain 1 1) 2)))


;;; feedback vermeiden!!!

(progn
  (let ((fn (lambda () (orgel-ctl 1 :level 1 (+ (mlevel 1 1) (gain 1 1) 2)))))
    (push fn (observed (mlevel 1 1)))
    (push fn (observed (gain 1 1)))))

(observed adds fn to responders of input)

(level 1 1)

(mlevel 1 1)

(let ((fn (lambda () (orgel-ctl 1 :level 4 (+ (mlevel 1 1) (gain 1 1) 2)))))
  (funcall fn))

:level04 (+ (mlevel 1 1) (gain 1 2) 2)

(format nil "~S" )

(defun key-and-idx (key)
  "split :level01 into the values :level and 1."
  (let* ((name (symbol-name key))
         (len (length name)))
    (values
     (intern (subseq name 0 (- len 2)) :keyword)
     (read-from-string (subseq name (- len 2) nil)))))

;;; (key-and-idx :level01) -> :level, 1

(let ((target :level01)
      (orgelnummer 1)
      (val 64))
  (multiple-value-bind (key idx)
      (key-and-idx target)
    (orgel-ctl orgelnummer key idx val)))


(defun make-ctl-fn (key idx expr)
  (let ((args (gethash key *orgel-preset-def-lookup*)))
    `(lambda () (,(first args) ,idx ,@(rest args) ,expr))))

(make-ctl-fn)

(defmacro test (key idx expr)
  `,(make-ctl-fn key idx expr))

(setf *debug* nil)
#'level
(funcall (test :level11 1 (+ (mlevel 1 1) -13)))

(let ((orgelidx 1))
  (mapcar #'funcall (orgel-registry-base-freq
                     (aref *osc-responder-registry* (1- orgelidx)))))

(let ((orgelidx 1))
  (push (test :base-freq 1 (+ (mlevel 1 1) -13))
        (orgel-registry-base-freq
         (aref *osc-responder-registry* (1- orgelidx)))))

(setf *debug* nil)
(setf *debug* t)


(symbol-function 'level)
(orgel-ctl 1 :level 1)

*curr-state*

(defmacro make-orgel-responders (targets)
  (let ((targets targets))
    `,(collect-orgel-responder-defs targets)))

;;; (make-orgel-responders (:level :delay :q :gain :osc-level))

(defmacro expand-args (targets)
  `(make-orgel-responders ,(symbol-value targets)))

(let ((orgel-idx 0) (stream *oscin*))
  (expand-args *orgel-fader-targets*))


(defun parse-targets (expr))

(defun get-responder-fn (target orgelidx)
  `(lambda (i f)
     (setf (aref (,(orgel-slot-name target) (aref *curr-state* ,orgelidx)) (round (1- i))) f)
     (mapcar #'funcall (,(read-from-string (format nil "orgel-registry-~a" target)) (aref *osc-responder-registry* ,orgelidx)))
     (if *debug* (format t "orgel~2,'0d: ~a ~a ~a~%" ,(1+ orgelidx) ,target (round i) f))))

(get-responder-fn :level 1)

(defmacro define-orgel-responder (target orgelidx)
  `(list ,target
         (incudine::make-osc-responder
          stream ,(format nil "/orgel~2,'0d/~a" (1+ orgelidx) target) "ff"
          ,(get-responder-fn target orgelidx))))

(define-orgel-responder :level 0)


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

(orgel-registry-level (aref *osc-responder-registry* 0))

(defmacro get-orgel-fader-responders (stream orgelidx targets)
  `(append
     ,@(loop for target in (symbol-value targets)
             collect `(define-orgel-fader-responder ,stream ,orgelidx ,target))))

(append
 (get-orgel-fader-responders *oscin* 0 *orgel-fader-targets*))



(define-orgel-responder *oscin* 0 :level)

(let ((i 0))
  (mapcar #'funcall
          (aref
           (orgel-registry-level
            (aref *osc-responder-registry*
                  0))
           i)))

(mapcar #'funcall (orgel-registry- (aref *osc-responder-registry* orgelidx)))

(defun collect-orgel-responder-defs (targets)
  `(list
     ,@(loop for name in targets
             collect `(define-orgel-responder ,name))))

(defmacro expand-args (targets)
  `(make-orgel-responders ,(symbol-value targets)))

(expand-args *orgel-fader-targets*)


(defun fake-it (targets)
  (let ((targets targets))
    (make-orgel-responders targets)))

(dolist (target '(:level))
  (let ((orgelidx 0) (stream *oscin*))
    (define-orgel-responder (symbol-value target))))

(define-orgel-responder :level)

(collect-orgel-responder-defs *orgel-fader-targets*)

(defmacro make-orgel-responders (targets)
  (let ((targets targets))
    `,(collect-orgel-responder-defs targets)))

(make-orgel-responders (:level :delay :q :gain :osc-level))







(define-orgel-responder :level)

(let ((orgelidx 0) (stream *oscin*))
  (loop for target in '(:level)
        do (define-orgel-responder target)))

(define-orgel-responder :level orgel-level)

(define-orgel-responder :level)

(orgel-slot-name :level)


(defun make-orgel-responders (orgelidx &optional (stream *oscin*))
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
  (dolist (target *orgel-single-targets*)
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

*osc-responder-registry*



(setf (aref (orgel-registry-level (aref *osc-responder-registry* 1)) 0) nil)
(setf (aref (orgel-registry-mlevel (aref *osc-responder-registry* 0)) 0) nil)

(push (lambda () (orgel-ctl 1 :level 1 (+ (mlevel 1 1) (level 2 1) 10)))
      (aref (orgel-registry-level (aref *osc-responder-registry* 1)) 0))

(push (lambda () (orgel-ctl 1 :level 1 (+ (mlevel 1 1) (level 2 1) 10)))
      (aref (orgel-registry-mlevel (aref *osc-responder-registry* 0)) 0))

(gethash :level01 *orgel-preset-def-lookup*)



(get-orgel-no :orgel01)



(setf *test* (eval '(lambda () (orgel-ctl 3 :level 3 (+ (gain 1 3) (gain 2 1) 10)))))

(funcall *test*)

(defparameter *test* (get-fn :level03 :orgel03 '(+ (gain 1 3) (gain 2 1) 10)))

(funcall *test*)


(funcall (first (aref (orgel-registry-level (aref *osc-responder-registry* 0)) 0)))

(let ((assigns (gethash :level01 *orgel-preset-def-lookup*))
      (orgelno 1))
  (funcall (first assigns) orgelno (second assigns) (third assigns) 64))

(orgel-ctl)

