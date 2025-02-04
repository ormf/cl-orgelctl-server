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

(ql:quickload "cl-orgelctl")
(in-package :cl-orgelctl)

(defparameter *test* (funcall (orgel-access-fn :delay) (aref *curr-state* 0)))

(let ((arr (funcall (orgel-access-fn :delay) (aref *curr-state* 0))))
  (dotimes (i 16) (setf (val (aref arr i)) 0.2)))

(defparameter *my-arr* (funcall (orgel-access-fn :delay) (aref *curr-state* 0)))

(aref *orgel-presets* 1)
*curr-state*
(store-orgel-preset)

(let ((arr (funcall (orgel-access-fn :delay) (aref *curr-state* 0))))
  (dotimes (i 16) (setf (val (aref arr i)) (random 1.0))))


(dotimes (i 16) (setf (val (aref *my-arr* i)) (random 1.0)))


(setup-ref-cell-hooks)

(setf (val (aref *my-arr* 0)) (random 1.0))
(dotimes (i 16)
  (let ((orgel-name "orgel01")
        (slot-key :delay)
        (faderno (1+ i))
        (val (random 1.0))
        (i i))
    (incudine::at (+ (incudine:now) (* 48000 i 0.01))
                  (lambda () (incudine.osc:message
                         (oscout (gethash "client1" *clients*))
                         (format nil "/~a/~a" orgel-name slot-key)
                         "ff"
                         (float faderno 1.0) (float val 1.0))))))

(incudine.osc:close *oscout5*)
(defparameter *oscout5* (incudine.osc:open :direction :output :port 32145 :protocol :tcp))
(defparameter *oscout6* (incudine.osc:open :direction :output :port 32146 :protocol :udp))

(incudine.osc:close *oscout6*)

(dotimes (i 160)
  (incudine.util:msg :warn "~a" i)
  (incudine.osc:message *oscout5* "/hello/world" "ff" (float i) 3.4)
)

(incudine::remove-all-responders cm:*midi-in1*)
(in-package :scratch)

(incudine.osc:close *oscout*)

(defparameter *oscout* (incudine.osc:open :direction :output :port 32145 :protocol :tcp))

(dotimes (i 160) (incudine.osc:message *oscout* "/hello/world" "ff" (float i) 3.4))
(setf (incudine.util::logger-level) :info)
(setf (incudine.util::logger-level) :warn)

(incudine:rt-start)

(incudine:rt-status)

(val (orgel-base-freq (aref *curr-state* 0)))
*midi-cc-state*

*midi-cc-responders*
*route-presets*

(incudine.osc:message *oscout* "/orgel01/level" "if" 1 (float 0.1 1.0))

(defparameter *testoscout* nil)
(defparameter *testoscout2* nil)

(setf *testoscout* (incudine.osc:open :host "127.0.0.1" :port 3015 :direction :output))
(setf *testoscout2* (incudine.osc:open :host "127.0.0.1" :port 3020 :direction :output))
(incudine.osc:message *testoscout* "/orgel01/level" "sif" "client1" 1 (float 0.1 1.0))
(incudine.osc:message *testoscout2* "/orgel01/level" "sif" "client1" 1 (float 0.1 1.0))

(incudine::make-osc-responder
 *oscin-lisp-server*
 (format nil "/orgel01/test") "sff"
          (lambda (client faderidx value)
            (incudine.util:msg :info "orgel01 in: ~S test ~a ~a~%" client  (round faderidx) value)))

(incudine::make-osc-responder
   *oscin-lisp-server* (format nil "/orgel01/level") "sff"
          (lambda (client faderidx value)
            (incudine.util:msg :info "orgel01 in: ~S ~a ~a ~a~%" client 'level (round faderidx) value)
))
*curr-state*
(format t "~a" (symbol-value :level))






(type-of 'orgel)

(get-orgel-fader-responders *oscin-lisp-server* 0 *orgel-fader-targets*)
(incudine:recv-start *oscin-lisp-server*)

(setup-ref-cell-hooks)
*clients*
(incudine.osc:message *testoscout* "/orgel01/level" "sff" "client1" (float 1 1.0) (float 0.8 1.0))

(incudine.osc:message *testoscout* "/orgel01/base-freq" "sf" "client1" (float (+ 200 (random 1000)) 1.0))
(orgel-name 1)
(aref *curr-state*)

(incudine.osc:message *testoscout* "/orgel01/delay" "sff" "client1" (float 1 1.0) (float 0.4 1.0))

(incudine.osc:message *testoscout* "/orgel01/test" "sff" "client1" (float 1 1.0) (float 0.1 1.0))

(incudine.osc:message *testoscout* "/connect-client" "si" "127.0.0.1" 3040)

(setf (incudine.util:logger-level) :info)

(orgel-ctl-fader :orgel01 :level 1 0.1)

(start-lisp-server)
(make-instance 'model-slot)

(push 'blah (orgel-dependencies ()))
(orgel-ctl-fader :orgel01 :level 1 0.3)
(aref *midi-cc-state* 5)

(incudine.osc:close *org-oscout*)
(defparameter
    *org-oscout*
  (incudine.osc:open
   :host "127.0.0.1"
   :port 3011
   :direction :output
   :protocol :udp))

(incudine.osc:close *org-oscout*)

(incudine.osc:message *org-oscout* "/plist-ctl/stop"  "")

(incudine.osc:message *org-oscout* "/orgel01/mlevel"  "fff" 0.2 2 0.5)

(incudine.osc:bundle)

(loop for )



(send-plist '())

(send-plist '((:level 2 2 0.5) (:level 1 2 0.5)))

(orgel-ctl-fader)

(incudine.util:msg :info "Hallo")
(intern (string-upcase (caar *global-targets*)) 'cl-orgelctl)

(dolist (target *global-targets*)
  (destructuring-bind (target orgelno partial value) target
    (orgel-ctl-fader orgelno target partial value)))



(read-from-string "common-lisp-user::level")

(send-plist '((level 1 1 1) (level 1 2 1) (level 2 1 1) (level 2 2 1) (level 1 3 1)
 (level 1 5 1) (level 1 4 1) (level 2 11 1) (level 2 13 1) (level 2 3 1)
 (level 1 15 1) (level 1 12 1) (level 2 7 1) (level 1 6 1) (level 2 5 1)
 (level 1 11 1)))

(send-plist '((level 2 2 0.5) (level 1 2 0.5)))

cl-orgelctl::*global-targets*

(read-from-string ":hallo")

(intern "HALLO" 'SYMBOL)


*global-targets*

(send-plist '((:level 2 2 0.5) (:level 1 2 0.5)))

(setf *global-targets* '((:level 1 1 1) (:level 1 2 1) (:level 2 1 1) (:level 2 2 1) (:level 1 3 1)
 (:level 1 5 1) (:level 1 4 1) (:level 2 11 1) (:level 2 13 1) (:level 2 3 1)
 (:level 1 15 1) (:level 1 12 1) (:level 2 7 1) (:level 1 6 1) (:level 2 5 1)
 (:level 1 11 1)))


(incudine.osc:send-bundle *org-oscout* 0)

(loop for i below (incudine.osc:required-values *org-oscout*)
      collect (incudine.osc:value *oscout* i))

*oscin*
(ccin 20)
(incudine:rt-start)

(start-osc-midi-receive)

(orgel-ctl :orgel01 :bias-pos 0.2)



(setf (val (slot-value (aref *curr-state* 0) 'bias-pos))
      0.7) ; => #<m 0.2>

(setf (bias-pos 1) 0.0)

(bias-pos 1) ; => 0.0

(setf (bias-pos 1) 0.6) ; => 0.6 (60.000004%)

(base-freq 1) ; => 55.0

(setf (base-freq 1) 72) ; => 72.0

(level 1 1) ; => 0.21 (21.0%)

(defparameter *chord1* '((1 3) (2 5)))

(ccin 0) ; => 0.6771653 (67.71653%)

(all-notes-off)

(all-faders-off '(:q :gain))

(all-faders-off :level :orgeln '(1))

(all-faders-off :level)

(setf )
(setf (ccin 0) 1) ; => 1 (1 bit, #x1, #o1, #b1)

(incudine.util::set-logger-level :info)


(let ((address "/orgel01/osc-level"))
  (loop for x below 1000 do (incudine.osc:message *oscout* address "if" (mod x 16) (float 0.0 1.0))))

(let ((address "/orgel01/osc-level"))
  (loop for x below 1000 do (incudine.osc:message *oscout* address "if" (mod x 16) (float (random 1.0) 1.0))))

(let ((address "/orgel01/osc-level"))
  (incudine:at (incudine:now) (lambda () (loop for x below 1000 do (incudine.osc:message *oscout* address "if" (mod x 16) (float (random 1.0) 1.0)))))
  (incudine:at (+ (incudine:now) 10000) (lambda () (loop for x below 1000 do (incudine.osc:message *oscout* address "if" (mod x 16) (float (random 1.0) 1.0))))))

(loop for x below 1000 do (incudine.osc:message *oscout* address "if" (mod x 16) (float (random 1.0) 1.0)))


(orgel-ctl)

*global-midi-channel* ; => 5 (3 bits, #x5, #o5, #b101)

(ccin 0 5)  ; => 1 (1 bit, #x1, #o1, #b1)
(ccin 0) ; => 1 (1 bit, #x1, #o1, #b1)y

(dolist (ton *chord1*)
  (setf (level (first ton) (second ton)) 0.7))

(copy-preset *curr-state* (aref *orgel-presets* 2))
(copy-preset *curr-state* (aref *orgel-presets* 0))
(save-presets)
(recall-preset 1)
(clear-routes)
(setf (main 1) 0.5)

(setf (ccin 7) 0.3)

(let ((target :hallo))
  (cond
    ((eql target :hallo)))

  )



*midi-cc-state*
*global-midi-channel*
(setf (ccin 0) 0.8)

(remove-all-cc-responders)

(replace-keywords '(:main (ccin 1)) 1)

(setf (ccin 0) 0.1)
(setf (ccin 1) 0.3)
(setf (val (aref (aref *midi-cc-state* 4) 0)) 0.2)

(funcall (first (aref (aref *midi-cc-responders* 4) 0)))

(orgel-ctl :orgel01 :bias-pos (ccin 1))

(ccin 1)

(bias-pos 0)

(bias-cos)

*curr-state*

(setup-ref-cell-hooks)

(slot-value (aref *osc-responder-registry* 0) (first *orgel-global-targets*))

(setup-ref-cell-hooks)

(start-keymap-note-responder)

(start-orgel-gui)
*

(setf (val (orgel-phase (aref *curr-state* 0))) -1)

(setf (ophase 1) -1.0)

(setf (ophase :orgel01) 1)

(symbol-function 'ophase)

(unintern 'bias-type)

(orgel-ctl :orgel01 :phase 1)

(level 1 1)


(setf (bias-pos 1) 14/15)

(incudine::remove-all-responders cm:*midi-in1*)

(setf (bias-type 1) -1)

(setf (level :orgel01 1) 0.9)

(orgel-ctl-fader :orgel01 :level 1 0.5)

(orgel-ctl :orgel01 :base-freq 275)
(orgel-ctl-fader :orgel01 :gain 10 0.5)

(ccin)

*base-freqs*

 ; => (27.5 32.401794 38.49546 46.19711 56.132587 69.28748 87.30706 113.156204
 ; 152.76933 220.0)

*orgel-freqs*

(defparameter *well-tempered-lookup* nil)

(progn
  (setf *well-tempered-lookup*
        (make-array 128 :element-type 'list :initial-element nil))
  (loop
    for entry across *well-tempered-lookup*
    for keynum from 0
    if (null entry)
      do (push (find-orgel-partial (mtof keynum)) entry)))

(progn
  (setf *well-tempered-lookup*
        (make-array 128 :element-type 'list :initial-element nil))
  (dolist (elem *orgel-freqs*)
    (push elem (aref *well-tempered-lookup* (round (second elem)))))
  (loop
    for entry across *well-tempered-lookup*
    for keynum from 0
    if (null entry)
      do (setf (aref *well-tempered-lookup* keynum) (list (find-orgel-partial (mtof keynum))))
    else do (print "found")))

https://hmwk.heconf.de/r?room=HMWK%3A+MPK-Digitalisierung

(find-orgel-partial 231)

(destructuring-bind (&optional a b c d) '()
  (list a b c d))

(defparameter *orgel-midi-responder*
  (incudine:make-responder
   cm:*midi-in1*
   (lambda (st d1 d2)
     (case (cm:status->opcode st)
       (:cc (let ((channel (cm:status->channel st))
                  (val (float (/ d2 127) 1.0)))
              (incudine::msg info "orgel-midi-responder: ~d ~d ~,2f" channel d1 val)
              (setf (ccin d1 channel) val)))))))

(setf (note-in 4 0) 0.5)

(orgel-name 1)

(let ((fn (lambda (amp keynum)
            (destructuring-bind (freq keynum orgelno faderno) (elt *orgel-freqs* keynum)
              (declare (ignore freq keynum))
              (orgel-ctl-fader (orgel-name orgelno) :osc-level faderno amp)))))
  (dotimes (chan 16)
    (dotimes (key 128)
      (add-note-responder key fn :channel chan))))

*midi-note-responders*

(funcall (first (elt (elt *midi-note-responders* 0) 0)) 0.0 0)
(ou:range 16)

(all-notes-off)

(remove-all-note-responders)


(funcall
 (lambda (amp keynum)
   (destructuring-bind (freq keynum orgelno faderno) (elt *orgel-freqs* keynum)
     (declare (ignore freq keynum))
     (orgel-ctl-fader (orgel-name orgelno) :osc-level faderno amp)))
 0.0 0)

(orgel-ctl-fader :orgel01 :osc-level  2 0.5)



(replace-keywords '(apply-notch :bias-type (bias-cos :bias-pos :bias-bw)) 1)
 ; => (apply-notch (bias-type 1) (bias-cos (bias-pos 1) (bias-bw 1)))

(bias-type 1)

(set-orgel-freqs
 (mapcar (lambda (x) (* x 2))
         '(27.5 32.401794 38.49546 46.19711 56.132587
           69.28748 87.30706 113.156204 152.76933 220.0))
 2)

(progn
  (set-orgel-freqs
   (mapcar (lambda (x) (* x 2))
           '(27.5 32.401794 38.49546 46.19711 56.132587
             69.28748 87.30706 113.156204 152.76933 220.0))
   2)
  (digest-route-preset
   15
   `(:preset nil
     :routes (:orgel01
              (:bias-pos (ccin 0) :bias-bw (ccin 1)
               :global ((apply-notch :bias-type
                                      (bias-cos :bias-pos :bias-bw
                                                :targets *global-targets*
                                                :levels *global-amps*))
                        *global-targets*))))))
(loop)
(set-global-faders *global-targets* (lambda (x) 0.3))

(setf *global-amps* #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(length *global-targets*)

(replace-keywords
 '(apply-notch :bias-type
   (bias-cos :bias-pos :bias-bw :targets *global-targets*
             :levels *global-amps*))
 1)

(setf *global-targets*                  
      '((level 1 1) (level 1 2) (level 2 1) (level 2 2) (level 1 3) 
        (level 1 5) (level 1 4) (level 2 11) (level 2 13) (level 2 3) 
        (level 1 15) (level 1 12) (level 2 7) (level 1 6) (level 2 5)
        (level 1 11)))

(bias-cos)

(defun bias-lin (bias-pos)
  (lambda (x)
    (format t "x: ~a~%" x)
    (if (=
         (round (* x 15))
         (round (* bias-pos 15)))
        x
        0)))

(defparameter *akkord* '(60 64 67 72))

(defun suche-fader (midi)
  (loop
    for ((freq1 midi1 orgelno1 faderno1)
         (freq2 midi2 orgelno2 faderno2))
      on *orgel-freqs*
    while freq2
    until (< midi1 midi midi2)
    finally (return (if (< (- midi2 midi) (- midi midi1))
                        (list freq2 midi2 orgelno2 faderno2)
                        (list freq1 midi1 orgelno1 faderno1)))))

(suche-fader 60)

(loop for keynum in *akkord*
      collect (cons 'osc-level (nthcdr 2 (suche-fader keynum))))

(orgel-ctl-fader 1 'osc-level 1 0.5)

(orgel-name 1) ; => :orgel01

(orgel-ctl-fader :orgel01 'osc-level 1 0.5)

(dotimes (orgel 10)
  (orgel-ctl
   (orgel-name (1+ orgel)) :base-freq
   (elt *base-freqs* orgel)))

(loop
  for keynum in *akkord*
  do (destructuring-bind
         (orgel-no fader-no) (nthcdr 2 (suche-fader keynum))
       (orgel-ctl-fader (orgel-name orgel-no) 'osc-level fader-no 1.0)))


(ats-cuda:browser-play-papierorgel ats-cuda::village01)
(play-browser 4)

(replace-keywords
 '(bias-cos :bias-pos :bias-bw :targets *global-targets*
   :levels *global-amps*)
 *global-targets*)

(bias-cos)


(eval `(lambda (&rest args) (declare (ignorable args))
               (set-global-faders ,(second form) ,(first form))))

(defun permute (fn permutation)
  ""
  (let ((array
          (loop for x across permutation
                for idx from 0 by (/ (1- (length permutation)))
                with array = (make-array (length permutation) :initial-element 0.0)
                do (setf (aref array (1- x)) (1+ idx))
                finally (return array))))
    (lambda (x) (funcall fn (aref array (1- x))))))

(funcall
 (permute (bias-cos (bias-pos 1) (bias-bw 1))
          #(1 16 2 15 3 14 4 13 5 12 6 11 7 10 8 9)) 2)



*curr-state*
(setf *debug* nil)
(setf *debug* t)
(set-faders :orgel01 :level (bias-cos (/ (1- 12) 15) 0.1))

#xFF

#b1011

(set-faders :orgel01 :level (lambda (x) x 0))
(set-faders :orgel01 :bias-level (lambda (x) x 0))
(funcall (bias-cos 2 4) 3)
(make-orgel-responder)

(orgel-ctl 1 :level 4 64)

(clip 17 15 16)

(ql:quickload "cl-plot")

(cl-plot:plot
 (let ((bw 1)
       (bias-pos 1))
   (lambda (x) (+ 0.5 (* 0.5 (cos (clip (* (/ (- x 0) 15.0) (/ bw)) (* -1 pi) pi))))))
 :region '(-1 16) :num-values 1000)

(cl-plot:plot
 (let* ((bw 1)
        (bias-pos 16)
        (fader-interp (- (clip bw 15 16) 15)))
   (lambda (x) (+ fader-interp
             (* (- 1 fader-interp) (+ 0.5 (* 0.5 (cos (clip (* pi (/ (- x bias-pos) 15) (+ 16 (* -1 bw)))
                                                           (* -1 pi) pi))))))
     )) :region '(0 17) :num-values 1000)

(progn
  (orgel-ctl-global 1 :base-freq 103.6)
  (orgel-ctl-global 1 :ramp-up 239.5)
  (orgel-ctl-global 1 :ramp-down 239.5)
  (orgel-ctl-global 1 :max-amp 1)
  (orgel-ctl-global 1 :exp-base 0.3)
  (dotimes (idx 16)
    (orgel-ctl 1 :gain (1+ idx) 64))
  (dotimes (idx 16)
    (orgel-ctl 1 :level (1+ idx) 0)))

(setf *debug* t)
(recall-preset 0)
(make-pathname)

(orgel- 1 :level 1 0)

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

(float (cm:interpl 100 (loop
                                                         for idx from 0
                                                         for val in
                                                         '(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 5 5 5 5 5 6 6 6 6 6 7 7 7 7 7 8 8 8 8 8 9 9 9 10 10 11 11 11 12 12 13 13 14 14 15 15 16 16 17 17 18 19 19 20 21 22 23 24 25 27 29 31 33 34 35 36 37 37 38 39 39 39 40)
                                                         with curr = -1
                                                         if (/= val curr) append
                                                                          (prog1 (list idx (* 100/40 val))
                                                                            (setf curr val)))) 1.0)


(setf (mlevel 1 1) 60)

(format t "[~{~,2f~^, ~}]"
        (loop for idx below 113 collect
                                (float (cm:interpl idx (loop
                                                         for idx from 0
                                                         for val in
                                                         '(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 5 5 5 5 5 6 6 6 6 6 7 7 7 7 7 8 8 8 8 8 9 9 9 10 10 11 11 11 12 12 13 13 14 14 15 15 16 16 17 17 18 19 19 20 21 22 23 24 25 27 29 31 33 34 35 36 37 37 38 39 39 39 40)
                                                         with curr = -1
                                                         if (/= val curr) append
                                                                          (prog1 (list idx (* 100/40 val))
                                                                            (setf curr val)))) 1.0)))

(format t "~%~%[~{~,2f~^, ~}]"
        (loop for idx below 113 collect
                                (float (cm:interpl idx '(0 0 100 145/2 112 100)) 1.0)))

(format t "[~{~,2f~^, ~}]"
        (loop for idx below 113 collect
                                (float (cm:interpl idx (loop
                                                         for idx from 0
                                                         for val in
                                                         '(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 5 5 5 5 5 6 6 6 6 6 7 7 7 7 7 8 8 8 8 8 9 9 9 10 10 11 11 11 12 12 13 13 14 14 15 15 16 16 17 17 18 19 19 20 21 22 23 24 25 27 29 31 33 34 35 36 37 37 38 39 39 39 40)
                                                         with curr = -1
                                                         if (/= val curr) append
                                                                          (prog1 (list idx (* 100/40 val))
                                                                            (setf curr val)))) 1.0)))


(/ 100 70)
(/ 80 1.12)
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

(setf *test* (make-instance 'model-slot))

(setf *test2* (make-instance 'value-cell :ref *test*))

(setf (val *test*) 30)

(setf (val *test2*) 45)

(orgel-ctl :orgel01 :ramp-up 23)
(orgel-ctl :orgel01 :main 0.03)

(orgel-ctl-fader :orgel01 :osc-level 8 0.0)
(orgel-ctl-fader :orgel01 :osc-level 8 0.4)

(orgel-ctl :orgel01 :base-freq 55)

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


(setf *debug* t)
clog-connection::*connection-data*

(let ((res '()))
  (maphash (lambda (key val) (push key res))
           clog-connection::*connection-data*)
  res)


*curr-state*


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
  (dolist (target *orgel-level-meter-targets*)
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

(orgel-ctl 1 :level 1 64)

(:preset 1
         :routes
          (:orgel01
           (:level01 (+ (mlevel 1 1) (delay 1 1) -14)
            :level02 (+ (level 1 1) (* -1 (q 1 1)) 5)
            :level03 (+ (level 1 2) (* -1 (q 1 2)) -13)
            :level04 (+ (level 1 3) 5))))

(parse-observed
 '(apply-notch (bias-type 1) (bias-wippe (bias-pos 1) (bias-bw 1))))

(replace-keywords
 '(apply-notch :level01 (bias-wippe :bias-pos :bias-bw)) 1)


(parse-observed '(apply-notch (level 2 1) (bias-cos (bias-pos 1) (bias-bw 1))))

(orgel-ctl-global 1 :bias-type +notch+)
(orgel-ctl-global 1 :bias-type +bandp+)

(orgel-ctl :orgel01 :phase +phase+)
(orgel-ctl :orgel01 :phase +invert+)

(orgel-ctl :orgel01 :level01 0.5)

(orgel-ctl :orgel01 :delay02 0.5)


(gethash :level01 )

(gethash :level01 *observed*)

(gethash :base-freq *observed*)

(let ((orgel-registry (aref *osc-responder-registry* 0)))
  (member (first (slot-value (aref *osc-responder-registry* 0) 'bias-pos))
          (slot-value orgel-registry 'bias-pos))
  )


(orgel-ctl :orgel01 :main 0.3)

(orgel-ctl :orgel01 :base-freq 411.7)

(orgel-ctl-fader :orgel01 :level 1 0.5)
(orgel-ctl-fader :orgel01 "osc-level" 1 0)

(symbol-function (orgel-slot-name (symbol-value :level)))

(setf (orgel-base-freq (aref *curr-state* 2)) (float 231))

(setf (slot-value (aref *curr-state* 2) 'base-freq) (float 265))

(lambda (f)
  (setf (,(orgel-slot-name (symbol-value target)) (aref *curr-state* ,orgelidx)) f)
  (mapcar #'funcall (slot-value (aref *osc-responder-registry* ,orgelidx)
                                ',(read-from-string (format nil "~a" (symbol-value target)))))
  (if *debug* (format t "orgel~2,'0d: ~a ~a~%" ,(1+ orgelidx) ,target f)))


  (setf (aref (,(orgel-slot-name (symbol-value target)) (aref *curr-state* ,orgelidx)) (round (1- i))) f)
            (mapcar #'funcall (aref
                               (slot-value (aref *osc-responder-registry* ,orgelidx)
                                          ',(read-from-string (format nil "~a" (symbol-value target))))
                               (round (1- i))))

(lambda (i f)
  (setf (aref (aref *orgel-mlevel* ,orgelidx) (round (1- i))) f)
  (mapcar #'funcall (aref
                     (slot-value (aref *osc-responder-registry* ,orgelidx)
                                 ',(read-from-string (format nil "~a" (symbol-value target))))
                     (round (1- i)))))

*curr-state*
*orgel-mlevel*
(setf *debug* t)

(make-all-responders *orgelcount* *oscin*)


(let ((result nil))
  (maphash (lambda (key val) (push (gethash "orgel-gui" val)
                              result))
           clog-connection::*connection-data*)
  (setf (clog::attribute (first (cl-orgel-gui::orgel-meters (aref (cl-orgel-gui::orgel-gui-orgeln (first result)) 0)))
                   "data-db") -100))

(setf *debug* nil)
(setf *debug* t)

(aref *curr-state* 0)

(aref *orgel-mlevel* 0)



*orgel-presets*

(recall-orgel-preset 0)

(model-orgel->val-orgel (aref *curr-state* 0))

(cl-orgelctl::store-orgel-preset)
*curr-state*

(let ((preset (elt *orgel-presets* 0)))
  (dotimes (idx *orgelcount*)
    (val-orgel->model-orgel (aref preset idx) (aref *curr-state* idx))))
(clog::value)
(copy-orgel)
(make-orgel-val-receiver)

(setf (cl-orgel-gui::orgel-gui-orgeln cl-orgel-gui::*curr-orgel-state*) *curr-state*)

(make-instance 'model-slot)

(orgel-ctl-fader :orgel01 :level 1 0.5)

(orgel-ctl :orgel01 :base-freq 55)

(let ((orgel (aref *curr-state* 0)))
  (setf (set-cell-hook (slot-value orgel 'base-freq)) (lambda (val) (format t "hook: base-freq set to ~a~%" val))))

(set-cell (slot-value (aref *curr-state* 0) 'base-freq) 231)

(orgel-ctl)

recall-orgel neu schreiben!

set-orgel-freqs
coords (ats-cuda) checken!

k.christ@hoefler-fenster.de
339x182

Vorderseite:

Gesamtbreite des rechten, großen Fensters: 340.2 cm
Gesamthöhe: 247.6 cm
Davon Brüstungshöhe (Blende unten): 65 cm
Glasfläche: 182.6 x 340.2

Die Gesamtbreite der Front ist 5,561 m, geteilt in 2,158 m Fenster links + Tür
und
3,402 m für das große Fenster rechts. Die Breite der Balkontür weiss ich leider
nicht, ich vermute eine Standardbreite von 0,900 m.

Rückseite:

Von außen sind die Aluprofile der Fensterrahmen beider Zimmer
verbunden (siehe angehängtes Foto von außen). Dabei beträgt der
Abstand zwischen den Aluminiumrahmen der Fenster beider Zimmer 14 cm.
Beim rechten Zimmer ist außerdem noch der Spalt zwischen Fensterrahmen
und der Wand rechts mit einem Holzprofil ausgefüttert, das 4cm breit
ist.




Bitte beachten Sie, dass die Maße nach bestem Wissen erstellt wurden,
aber unverbindlich sind und vor Auftragsausführung/Herstellung von den
Handwerkern vor Ort zu überprüfen sind!

Ich bitte auch zu beachten, dass bei dem Haus eine äußere Wärmedämmung
geplant ist, die bei den Fenstermaßen entsprechend berücksichtigt
werden sollte. Nach meinem Verständnis beduetet das, dass ganz links und ganz rechts an der Wand eine ca. 10 cm breite Blende
angebracht werden sollte, da das Haus noch eine Außenwärmedämmung. D.h. das
Fenster rechts ist gar nicht 3,402 m breit, sondern um Blende bzw. Rahmen
schmaler. Beiliegend eine Skizze. Reicht Ihnen das so?


(defparameter *test* (make-instance 'cellctl::model-slot))

(defparameter *test2* (make-instance 'cellctl::value-cell :ref *test*))

(defun main-group (level)
  "return a function which calculates the level level for a slider
[1-(length targets)] with given center freq and bw. bias-pos and bw
are normalized. bw is the distance between the bias-pos and the -6 dB
points left/right of the bias-pos. At 15/15.5<bw<1 the values of the
faders are interpolated between the faders at bw 15/15.5 and 1."
  (let* ()
    (lambda (x) (declare (ignore x)) level)))

(setf (notein 0) 12)

(setf (cl-orgelctl::notein 10) 54)

(aref *midi-note-state* 5)

(setf (incudine.util:logger-level) :info)

(notein 96)

(parse-observed '(cl-orgelctl::ccin 60))


(defun split-str (string &optional (separator " "))
  (split-1 string separator))

(defun split-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

(defun split-str (string &optional (separator " "))
  (labels ((split (str acc)
             (cond
               ((or (null str) (string= str "")) acc)
               (t (let ((n (position separator str)))
                    (split (subseq str (1+ n)) (cons (subseq str 0 n) acc)))))))
    (split string nil)))

(defun split-str (string &optional (separator " "))
  (loop
    for str = string then (subseq str (1+ n))
    for n = (position separator str)
    while n
    collect (subseq str 0 n)))

(split-str "Aber Aber  Herr Nachbar")

(reduce #'+ '(1 2 3 4 5))
