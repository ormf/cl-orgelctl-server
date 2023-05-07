;;; 
;;; ats-player.lisp
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

(defun browser-play-papierorgel (ats-sound &rest args)
  (let* ((ats-snd
           (if (or (stringp ats-sound) (typep ats-sound 'pathname))
               (symbol-value (ats-cuda::ats-load
                              ats-sound
                              (intern
                               (string-upcase
                                (pathname-name (pathname ats-sound)))
                               :ats-cuda)))
               ats-sound))
         (num-partials (ats-sound-partials ats-snd))
         (maxfreq (float (+ 100 (aref (ats-sound-frq-av ats-snd) (1- num-partials))) 1.0))
         (browser-player
           (make-browser-player
            :ats-sound ats-snd
            :amp-scale (getf args :amp-scale 1.0)
            :num-partials num-partials
            :partials (getf args :partials (range num-partials))
            :res-bal (getf args :res-bal 0.5)
            :maxfreq maxfreq
            :last-frame -1
            :amod (make-array num-partials :element-type 'incudine::sample :initial-element 1.0d0)
            :fmod (make-array num-partials :element-type 'incudine::sample :initial-element 1.0d0)
            :bw (getf args :bw 40000)
            :soundpos (getf args :soundpos 0)
            :mousefreq (* (max 0.0 (min (getf args :y 0) 1.0)) maxfreq))))
    (ats->svg ats-snd :brightness (getf args :brightness 20))
    (broadcast-message "reload")
    (if *curr-browser-player* (free (browser-player-id *curr-browser-player*)))
    (setf *curr-browser-player* browser-player)
    (recalc-amps)
    ;; (apply #'incudine::sin-noi-rtc-synth 0.0 ats-snd
    ;;        :amod (browser-player-amod browser-player)
    ;;        :fmod (browser-player-fmod browser-player) :id (getf args :id 2) args)
    (setf (browser-player-id *curr-browser-player*) (getf args :id 2))
    browser-player))


(defun get-freq-amps (frame ats-sound &key (dbthresh -40))
  (loop
    with ampthresh = (ou:db->amp dbthresh)
    for partial below (ats-sound-partials ats-sound)
    for amp = (aref (ats-sound-amp ats-sound) partial frame)
    if (> amp ampthresh)
      collect (list (aref (ats-sound-frq ats-sound) partial frame) amp)))

(defun update-fader-amps (fader-amps browser-player)
  (let ((last-fader-amps (browser-player-last-fader-amps browser-player))
        (note-offs nil))
    (loop for fader in (first last-fader-amps)
          do (unless (member fader fader-amps :test #'equal) (push fader note-offs)))
    (setf (browser-player-last-fader-amps browser-player) fader-amps)
    (if note-offs
        (mapcar #'append fader-amps (list note-offs (ou:n-collect (length note-offs) 0.0d0)))
        fader-amps)))

(defun coords (x y)
  (let* ((ats-sound (browser-player-ats-sound *curr-browser-player*))
         
         (frame (round (* x (1- (ats-sound-frames ats-sound))))))
    (if (/= frame (browser-player-last-frame *curr-browser-player*))
        (let ((fader-amps (cl-orgelctl::find-orgel-fader-amps
                           (get-freq-amps frame ats-sound)
                           :fader 'cl-orgelctl::osc-level)))
          (setf (browser-player-last-frame *curr-browser-player*) frame)
;;;          (format t "~&faders: ~a~%" fader-amps)
          (setf cl-orgelctl::*global-targets* (first fader-amps))
          (setf cl-orgelctl::*global-amps* (coerce (second fader-amps) 'vector))
          (cl-orgelctl::oscillator-mute)
          (mapcar #'funcall (slot-value (aref cl-orgelctl::*osc-responder-registry* 0) 'cl-orgelctl::bias-bw))))
    (cl-orgelctl::orgel-ctl :orgel01 :bias-pos y))
  ;; (set-control 2 :soundpos x)
  ;; (setf (browser-player-soundpos *curr-browser-player*) x)
  ;; (setf (browser-player-mousefreq *curr-browser-player*)
  ;;       (* (max 0.0 (min y 1.0)) (browser-player-maxfreq *curr-browser-player*)))
  ;; (recalc-amps)
  )


(export 'browser-play-papierorgel 'ats-cuda)

(in-package :cl-orgelctl)

#|
(ats-cuda:browser-play-papierorgel ats-cuda::village01)



ats-cuda::*curr-browser-player*
(coords)
(ats-cuda:browser-play-papierorgel ats-cuda::village01)

ats-cuda::*curr-browser-player*
|#