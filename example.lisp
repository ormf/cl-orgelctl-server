;;; 
;;; example.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :ats-cuda)

(defparameter village01 nil)
;;; (defparameter village02a nil)
;;; (defparameter village02b nil)

(tracker (asdf:system-relative-pathname :cl-orgelctl "snd/village01.wav")
           'village01
           :start 0.0
           :hop-size 1/4
           :lowest-frequency 100.0
           :highest-frequency 20000.0
           :frequency-deviation 0.5
           :lowest-magnitude (db-amp -40)
           :SMR-continuity 0.7
           :track-length 6
           :min-segment-length 3
           :residual "/tmp/village01-res.snd"
           :verbose nil
           :debug nil)

(in-package :cl-orgelctl)

(ats-cuda:browser-play-papierorgel ats-cuda::village01)

(progn
  (set-orgel-freqs
   (mapcar (lambda (x) (* x 4))
           '(27.5 32.401794 38.49546 46.19711 56.132587 69.28748 87.30706 113.156204 152.76933 220.0))
   2)
  (orgel-ctl :orgel01 :bias-bw 1)
  (digest-route-preset
   15
   `(:preset 2
     :routes (:orgel01
              (:bias-pos (ccin 0) :bias-bw (ccin 1) :global
                         ((apply-notch :bias-type
                                       (bias-cos :bias-pos :bias-bw :targets *global-targets*
                                                 :levels *global-amps*))
                          *global-targets*)))))
  (play-browser 4))

