;;; 
;;; osc-midi.lisp
;;;
;;; code to enable remote machines to send/receive midi messages over
;;; osc. The osc receiving port is 3011 (defined in osc.lisp), the
;;; sending port is 3010.
;;;
;;; The osc messages have these osc formats:
;;;
;;; "/ctlin" iii  (controller-value controller-number midi-channel)
;;; "/notein" iii  (keynum velocity midi-channel)
;;; "/pgmin" ii  (program-number midi-channel)
;;; "/bendin" ii  (bend-value midi-channel)
;;;
;;; in addition, the message:
;;;
;;; "osc-midi-register" s (host-ip)
;;;
;;; registers the ip address of a remote computer for sending outgoing
;;; midi messages via osc.
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

(in-package :cl-orgelctl)

(defparameter *osc-midi-registry* nil)

(defun make-osc-midi-responders (stream)
  (incudine::make-osc-responder
   stream "/ctlin" "fff"
   (lambda (value ccnum channel)
     (setf (ccin (round ccnum) (round (1- channel))) (/ value 127))
     (incudine::msg :info "ctlin: ~a ~a ~a" value ccnum channel)))
  (incudine::make-osc-responder
   stream "/notein" "fff"
   (lambda (keynum velo channel)
     (setf (notein (round keynum) (round (1- channel))) (/ velo 127))
     (incudine::msg :info "notein: ~a ~a ~a" keynum velo channel)))
  (incudine::make-osc-responder
   stream "/osc-midi-register" "s"
   (lambda (host)
     (pushnew host *osc-midi-registry* :test #'string-equal)
     (incudine::msg :info "osc-midi-register: ~a" host))))

(defun clear-osc-midi-registry ()
  (setf *osc-midi-registry* nil))
