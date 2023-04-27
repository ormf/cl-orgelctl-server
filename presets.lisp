;;; 
;;; presets.lisp
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

;;; *orgel-presets* currently just contains the static positions of
;;; all faders/numboxes, but no routing/relation of faders!

(defparameter *orgel-presets*
  (make-array
   128
   :initial-contents
   (loop
     for i below 128
     collect (make-array
              *num-orgel*
              :initial-contents
              (loop
                for x below *num-orgel*
                collect (make-orgel))))))

(defun copy-preset (src target)
  (dotimes (i *num-orgel*)
    (setf (aref target i)
          (copy-orgel (aref src i)))))

;;; (copy-preset *curr-state* (aref *orgel-presets* 0))
;;;(aref *curr-state* 1)

(defun recall-orgel (orgelidx num &optional next interp)
  "recall the complete state of orgel at orgelidx from preset <num> by
sending the values using osc. <interp> is a value between 0 and 1
interpolating all values between presets <num> and <next>."
  (dolist (slot '(:level :delay :q :gain :osc-level)) ;;; sliders
    (dotimes (i 16) ;;; iterate over all 16 sliders
      (let ((val (if next
                     (+
                      (* (if interp (- 1 interp) 0.5)
                         (aref (funcall (orgel-access-fn slot)
                                        (aref (aref *orgel-presets* num) orgelidx))
                               i))
                      (* (or interp 0.5)
                         (aref (funcall (orgel-access-fn slot)
                                        (aref (aref *orgel-presets* next) orgelidx))
                               i)))
                     (aref (funcall (orgel-access-fn slot)
                                    (aref (aref *orgel-presets* num) orgelidx))
                           i))))
        (if *debug* (format t "sending: orgel~2,'0d: ~a ~a ~a~%" (1+ orgelidx) slot (1+ i) val))
        (orgel-ctl-fader (orgel-name (1+ orgelidx)) slot (1+ i) val))))
  (dolist (slot *orgel-global-targets*) ;;; global slots
    (let ((val (if next
                    (+
                     (* (if interp (- 1 interp) 0.5)
                        (funcall (orgel-access-fn slot)
                                 (aref (aref *orgel-presets* num) orgelidx)))
                     (* (or interp 0.5)
                        (funcall (orgel-access-fn slot)
                                 (aref (aref *orgel-presets* next) orgelidx))))
                    (funcall (orgel-access-fn slot) (aref (aref *orgel-presets* num) orgelidx)))))
      (if *debug* (format t "sending: orgel~2,'0d: ~a ~a~%" (1+ orgelidx) slot val))
      (orgel-ctl (orgel-name (1+ orgelidx)) slot val))))

(defun recall-preset (num &optional next interp)
  (when num
      (loop for orgel below *num-orgel*
            for time from 0 by 0.005
            do (let ((orgel orgel))
                 (cm::at (+ (cm:now) time) (lambda () (recall-orgel orgel num next interp)))))))

;;; (recall-preset 1)

(defun save-presets (&optional (file "/tmp/orgel-presets.lisp"))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (format out "(setf *orgel-presets* ~%~a)" *orgel-presets*)))

;;; (save-presets)

(defun load-presets (file)
  (load file))

;;; (orgel-ctl 1 :level 1 (random 128))


(defparameter *route-presets* (make-array 128 :initial-element nil :element-type 'list))

(defun digest-route-preset (preset-num form &key (reset t))
  (setf (aref *route-presets* preset-num) form)
  (recall-preset (getf form :preset))
  (digest-routes (getf form :routes) :reset reset))

(defun save-route-presets (&optional (file "./presets/route-presets.lisp"))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (format out "(in-package :cl-orgelctl)~%~%(setf *route-presets*~%~S)" *route-presets*)))

(defun load-route-presets (&optional (file "./presets/route-presets.lisp"))
  (load file))

(defun recall-route-preset (num)
  (let ((form (aref *route-presets* num)))
    (recall-preset (getf form :preset))
    (digest-routes (getf form :routes))
))

;;; (recall-route-preset 0)
;;; (save-route-presets)

#+swank
(defparameter *emcs-conn* swank::*emacs-connection*)

#+swank
(defun define-elisp-code ()
  (let ((swank::*emacs-connection* *emcs-conn*))
    (swank::eval-in-emacs
     `(progn
        (setq orgel-preset-file ,(namestring (merge-pathnames "curr-preset.lisp" (asdf:system-source-directory :cl-orgelctl))))
        (find-file orgel-preset-file)
        (set-window-dedicated-p (get-buffer-window "curr-preset.lisp" t) t)
        (load ,(namestring (merge-pathnames "edit-orgel-presets.el" (asdf:system-source-directory :cl-orgelctl))))
        ) t)))

#+slynk
(defun define-elisp-code ()
  (slynk::eval-in-emacs
   `(progn
      (setq orgel-preset-file ,(namestring (merge-pathnames "curr-preset.lisp" (asdf:system-source-directory :cl-orgelctl))))
      (find-file orgel-preset-file)
      (set-window-dedicated-p (get-buffer-window "curr-preset.lisp" t) t)
      (load ,(namestring (merge-pathnames "sly-edit-orgel-presets.el" (asdf:system-source-directory :cl-orgelctl))))
      ) t))


(defun preset->string (preset-form ref)
  (format nil "(digest-route-preset~%~d~%`(~S ~d~%~S ~S))"
          ref
          (first preset-form)
          (second preset-form)
          (third preset-form)
          (fourth preset-form)))

#+swank
(defun edit-preset-in-emacs (ref &key (presets *route-presets*))
  "send the preset form referenced by <ref> to emacs for display in the
curr-preset.lisp buffer."
  (let ((swank::*emacs-connection* *emcs-conn*))
    (if (numberp ref)
        (swank::eval-in-emacs
         `(edit-orgelctl-preset
           ,(progn
              (in-package :cl-orgelctl)
              (defparameter swank::*send-counter* 0)
              (preset->string (aref presets ref) ref))
           ,ref) t)
        (swank::eval-in-emacs
         `(save-excursion
           (switch-to-buffer (get-buffer "curr-preset.lisp"))) t))))

#+slynk
(defun edit-preset-in-emacs (ref &key (presets *route-presets*))
  "send the preset form referenced by <ref> to emacs for display in the
curr-preset.lisp buffer."
  (if (numberp ref)
      (slynk::eval-in-emacs
       `(edit-orgelctl-preset
         ,(progn
            (in-package :cl-orgelctl)
            (preset->string (aref presets ref) ref))
         ,ref) t)
      (slynk::eval-in-emacs
       `(save-excursion
         (switch-to-buffer (get-buffer "curr-preset.lisp"))) t)))

(define-elisp-code)

(defparameter *curr-preset-nr* 0)
(defparameter *max-preset-nr* 127)

(defun next-preset ()
  (if (< *curr-preset-nr* *max-preset-nr*)
      (edit-preset-in-emacs (incf *curr-preset-nr*))))

(defun previous-preset ()
  (if (> *curr-preset-nr* 0)
      (edit-preset-in-emacs (decf *curr-preset-nr*))))

(previous-preset)
(next-preset)
