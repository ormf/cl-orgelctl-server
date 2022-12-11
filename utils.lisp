;;; 
;;; utils.lisp
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

(defun r-elt (seq)
  (elt seq (random (length seq))))

(defun reset-orgel-global ()
  (loop for orgel from 1 to 6 do
    (progn
      (orgel-ctl-global orgel :ramp-up 249)
      (orgel-ctl-global orgel :ramp-down 249)
      (orgel-ctl-global orgel :exp-base 0.3)
      (orgel-ctl-global orgel :min-amp 0)
      (orgel-ctl-global orgel :max-amp 1)
      (orgel-ctl-global orgel :phase 1)
      (orgel-ctl-global orgel :base-freq 117))))

;;; (reset-orgel-global)

(defun orgel-slot-fn (slot)
  "get the access function of slot keyword."
  (symbol-function (read-from-string (format nil "orgel-~a" slot))))

;;; (orgel-slot-fn :level)

(defun orgel-nr (key)
  (getf *orgel-nr-lookup* key))

(defun orgel-name (idx)
  (aref *orgel-name-lookup* idx))

(defun clip (x min max)
  "clip x to the interval [min max]"
  (min max (max x min)))


(defun set-faders (orgel target fn)
  "set all faders of <target> at orgel <orgelno> to the values
determined by fn, called on all partials."
  (loop
    for fader from 1 to 16
    do (orgel-ctl-fader orgel target fader (funcall fn fader))))

(defun apply-notch (bias-type fn)
  "return a function by composing fn with an inverter of values with
respect to the range [0..127] (0->127, 127->0, 64->63, 63->64) if
(= bias-type 1), otherwise don't invert."
  (if (= bias-type 1)
      (lambda (x) (+ 1 (* -1 (funcall fn x))))
      fn))

(defmacro n-exp (x min max)
  (let ((quot (if (zerop min) 0 (/ max min))))
    `(if (zerop ,x)
         ,min
         (* ,min (expt ,quot ,x)))))

(defmacro n-lin (x min max)
  (let ((diff (- max min)))
    `(+ ,min (* ,diff ,x))))

(expt 0 1)

(defun recalc-bw (bw)
  (n-lin bw 0.5 16))

(defun recalc-bias-pos (pos)
  (n-lin pos 1 16))

(defun bias-cos (bias-pos bw &key (levels #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
  "return a function which calculates the bias level for a slider [1-16]
with given center freq and bw. bias-pos and bw are normalized. bw is
the distance between the bias-pos and the -6 dB points left/right of
the bias-pos. At 15/15.5 <bw<1 the faders are interpolated between the
faders at bw 15/15.5 and max level of all faders at bw 1."
  (let* ((real-bw (recalc-bw bw))
         (fader-interp (- (clip real-bw 15 16) 15)))
    (lambda (x) (* (aref levels (1- x))
              (+ fader-interp
                 (* (- 1 fader-interp)
                    (+
                     0.5
                     (* 0.5
                        (cos
                         (clip (/ (* pi 1/2 (- x (recalc-bias-pos bias-pos)))
                                  real-bw)
                               (* -1 pi) pi))))))))))

(defun bias-wippe (bias-pos bw &key (levels #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
  (lambda (x) (let* ((real-bw (+ (* 18/33 (1- (n-lin bw 0.5 15.5))) 7))
                (val1 (clip (+ 1 (* (1- x) (/ -1 real-bw))) 0 1))
                (val2 (clip (+ 1 (* (- 16 x) (/ -1 real-bw))) 0 1))
                (interp (/ (1- (recalc-bias-pos bias-pos)) 15)))
           (* (aref levels (1- x)) (+ (* (- 1 interp) val1) (* interp val2))))))

(defun bias-db-linear (bias-pos bw &key (levels #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
  (lambda (x) (let* ((real-bw (+ (* 18/33 (1- (n-lin bw 0.5 15.5))) 7))
                (val1 (clip (+ 1 (* (1- x) (/ -1 real-bw))) 0 1))
                (val2 (clip (+ 1 (* (- 16 x) (/ -1 real-bw))) 0 1))
                (interp (/ (1- (recalc-bias-pos bias-pos)) 15)))
           (* (aref levels (1- x)) (+ (* (- 1 interp) val1) (* interp val2))))))


(defun bias-wippe-db (bias-pos bw)
  (lambda (x) (let* ((real-bw (+ (* 9/15 (1- (recalc-bw bw))) 7))
                (val1 (clip (+ 1 (* (1- x) (/ -1 real-bw))) 0 1))
                (val2 (clip (+ 1 (* (- 16 x) (/ -1 real-bw))) 0 1))
                (interp (/ (1- (recalc-bias-pos bias-pos)) 15)))
           (* 127 (+ (* (- 1 interp) val1) (* interp val2))))))

(defun clear-routes ()
  (digest-routes nil))
