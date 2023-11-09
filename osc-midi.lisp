;;; 
;;; osc-midi.lisp
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

(defparameter *local-host* "127.0.0.1")

;;; this has to be defined for every registered host:
;;; (defparameter *oscout* (incudine.osc:open :port 4711 :host *remote-host* :direction :output :protocol :udp))
(defparameter *osc-midi-in* nil)
(defparameter *midictl-osc-responders* (make-hash-table)) ;;; a hashtable with the handles of all registered midi responders

(defparameter *midictl-osc-remote-connections* nil)

(defun osc-midi-add-remote-connection (host &key (port 4710))
  (let ((entry (first (member host *midictl-osc-remote-connections* :key #'incudine.osc:host :test #'string-equal))))
    (when entry
      (incudine.osc:close entry)
      (setf *midictl-osc-remote-connections*
            (delete entry *midictl-osc-remote-connections*))))
  (let ((connection (incudine.osc:open :port port :host host :direction :output :protocol :udp)))
    (push connection *midictl-osc-remote-connections*)
    (incudine.osc:message connection "/msg" "s" "connected")))

(defun osc-midi-remove-remote-connection (host)
  (let ((entry (first (member host *midictl-osc-remote-connections* :key #'incudine.osc:host :test #'string-equal))))
    (when entry
      (incudine.osc:close entry)
      (setf *midictl-osc-remote-connections*
            (delete host *midictl-osc-remote-connections* :key #'incudine.osc:host :test #'string-equal)))))

(defun osc-midi-remove-all-remote-connections ()
  (dolist (conn *midictl-osc-remote-connections*)
    (incudine.osc:close conn))
  (setf *midictl-osc-remote-connections* nil))

;;; osc responder:

(defun start-osc-midi-receive (&key (port 4711))
  "start osc on localhost:port and its receivers."
  (when *osc-midi-in* (incudine.osc:close *osc-midi-in*))
  (maphash (lambda (key val) key (incudine:remove-responder val)) *midictl-osc-responders*)
  (setf *osc-midi-in* (incudine.osc:open :host "127.0.0.1" :port port :direction :input :protocol :udp))
  (setf (gethash :osc-midi-register *midictl-osc-responders*)
        (incudine::make-osc-responder
         *osc-midi-in* "/osc-midi-register" "sf"
         (lambda (host port)
           (let ((port (round port)))
             (incudine.util:msg :info "osc-midi-register: ~a ~a" host port)
             (osc-midi-add-remote-connection host :port port)))))
  (setf (gethash :midiin *midictl-osc-responders*)
        (incudine::make-osc-responder
         *osc-midi-in* "/midiin" "fff"
         (lambda (st d1 d2)
           (let ((st (round st))
                 (d1 (round d1)))
             (case (cm:status->opcode st)
               (:cc (let ((channel (cm:status->channel st))
                          (val (float (/ d2 127) 1.0)))
                      (incudine::msg info "orgel-midi-responder: ~d ~d ~,2f" channel d1 val)
                      (setf (ccin d1 channel) val))))))))
  (incudine:recv-start *osc-midi-in*))

(defun stop-osc-midi-receive (&optional local-midi-in)
  (declare (ignore local-midi-in))
   (incudine:recv-stop *osc-midi-in*))

(defun osc-midi-broadcast (st d1 d2)
  (dolist (connection *midictl-osc-remote-connections*)
    (incudine.osc:message connection "/midiout" "iii" st d1 d2)))

(defun osc-midi-write-short (stream st d1 d2)
  (incudine.util:msg :info "osc-midi-write: ~a ~a ~a ~%" st d1 d2)
  (jackmidi:write-short stream (jackmidi:message st d1 d2) 3)
  (osc-midi-broadcast st (round d1) (round d2)))
