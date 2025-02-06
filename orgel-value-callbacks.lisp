;;; 
;;; orgel-value-callbacks.lisp
;;;
;;; callback functions when a value changes (triggered by osc messages
;;; or by fader movements in the html gui).
;;;
;;; the callback does 3 things:
;;;
;;; 1. update the respective slot of *curr-state*
;;; 2. set the faders in the html gui
;;; 3. set the values on the pd side
;;;
;;; the src argument is nil if the values are received from osc and
;;; therefore the callbacks don't send to pd (another mechanism has to
;;; be implemented if the patch should also receive osc messages from
;;; other sources than the pd patch).
;;;
;;; the src argument otherwise contains the html element which caused
;;; the value change to prevent a loop in the value changes to the
;;; connected html gui instances.
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

;;; all the callback functions are installed in:
;;;
;;; - osc.lisp (define-orgel-fader-responder...)
;;;
;;; - orgel-gui-redefs.lisp (make-orgel-val-receiver, etc., used in
;;;   the init routines of the gui widgets (init-vslider...) in
;;;   utils.lisp). They simply set the cell of the model-slots in
;;;   *curr-state*, triggering gui-redraw and behaviour defined in
;;;   route-presets (like controller actions and such with
;;;   digest-route-preset).

(defun orgel-global-value-callback (orgelidx target value src)
  "callback function if a global value changes (either through gui
interaction or by responding to osc events)."
  (set-cell (slot-value (aref *curr-state* orgelidx) target) value :src src))

(defun orgel-fader-value-callback (orgelidx target faderidx value src)
  "callback function if a fader value changes (either through gui
interaction or by responding to osc events)."
  (let ((f-idx (round (1- faderidx))))
    (set-cell (aref (slot-value (aref *curr-state* orgelidx) target) f-idx) value :src src)))

(defun orgel-mlevel-value-callback (orgelidx faderidx value src)
  "callback function if a meter level value changes (by responding to osc
events from the dsp engine)."
  (let ((f-idx (round (1- faderidx))))
    (set-cell (aref (aref *orgel-mlevel* orgelidx) f-idx) value :src src)))

(defun setup-ref-cell-hooks ()
  "Set up propagating changes in the model-slots of *curr-state* and
*orgel-mlevel* to all connected clients (gui and pd via osc). This
 also takes care of calling the route functions defined in presets, so
 triggering all necessary respond actions to gui, cc or osc messages
 boils down to simply setting the ref-cell in *curr-state*."
  (format t "setup-ref-cell-hooks!~%")
  (dotimes (orgelidx *orgelcount*)
    (let ((global-orgel (aref *curr-state* orgelidx)))
      (map
       nil
       (lambda (slot-sym slot-key)
         (let* ((orgelidx orgelidx)
                (db (if (eql slot-key :main) -60)))
           (setf (set-cell-hook (slot-value global-orgel slot-sym))
                 (lambda (val &key src)
                   (declare (ignorable src))
;;;                   (if val (global-to-pd orgel-name slot-key val))
                   (maphash
                    (lambda (key client)
                      (unless (equal key src)
                        (let ((slot-name (target-key->string slot-key))
                              (src "orgel-server"))
                          (incudine.util:msg :info "ref-cell-hook: sending to ~a: /orgelctl ~a ~S ~S ~a" key (float (1+ orgelidx) 1.0) src slot-name (float val 1.0))
                          (incudine.osc:message (oscout client) "/orgelctl" "sfsf" src (float (1+ orgelidx) 1.0) slot-name (float val 1.0)))))
                    *clients*)
                   (let ((val-string (format nil "~,3f" (if db (apply #'amp->ndb-slider val
                                                                      (if (numberp db) `(:min ,db)))
                                                            val)))
                         (attribute (if (member slot-key '(:bias-type :phase)) "data-val")))
                     (maphash (lambda (connection-id connection-hash)
                                (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                                (let* ((orgel-gui (gethash "orgel-gui" connection-hash)))
                                  (when orgel-gui (let ((elem (slot-value
                                                               (aref (orgel-gui-orgeln orgel-gui) orgelidx)
                                                               slot-sym)))
                                                    (unless (or (equal src elem) (not elem))
                                                      (if attribute
                                                          (setf (clog:attribute elem attribute) val-string)
                                                          (setf (clog:value elem) val-string)))))))
                              clog-connection::*connection-data*)) ;;; call the defined route functions
                   (dolist (fn (slot-value (aref *route-responder-registry* orgelidx) slot-sym)) ;;; call fns established with route presets.
                     (funcall fn val))))))
           *orgel-global-target-syms*
           *orgel-global-targets*)

      (map nil (lambda (slot-sym slot-key)
                 (let* ((orgelidx orgelidx)
                        (db (member slot-key '(:level :gain :osc-level))))
                   (dotimes (faderidx 16)
                     (let* ((faderidx faderidx)
                            (faderno (float (1+ faderidx) 1.0))
                            (db db))
                       (setf (set-cell-hook (aref (slot-value global-orgel slot-sym) faderidx))
                             (lambda (val &key src)
                               (declare (ignorable src))
;;;                               (format t "setup-ref-cell-hooks, slot-sym: ~a, fader-idx: ~a, val: ~a~%" slot-sym faderidx val)
                               (maphash
                                (lambda (key client)
                                  (unless (equal key src)
                                    (let ((slot-name (target-key->string slot-key))
                                          (src "orgel-server"))
                                      (incudine.util:msg :info "ref-cell-hook: sending to ~a: /orgelctlfader ~S ~a ~S ~a ~a" key src (float (1+ orgelidx) 1.0) key slot-name (float faderidx 1.0) (float val 1.0))
                                      (incudine.osc:message (oscout client) "/orgelctlfader" "sfsff" src (float (1+ orgelidx) 1.0) slot-name faderno (float val 1.0)))))
                                *clients*)
;;;                               (if val (fader-to-pd orgel-name slot-key (1+ faderidx) val))
                               (let ((val-string (format nil "~,3f" (if db (amp->ndb-slider val) val))))
                                 (maphash (lambda (connection-id connection-hash)
                                            (declare (ignore connection-id))
                                            (incudine.util:msg :info "setting: ~S ~S ~a to ~a (from ~a) in gui" (orgel-name (1+ orgelidx)) slot-key (1+ faderidx) val-string val)
                                            (let* ((orgel-gui (gethash "orgel-gui" connection-hash)))
                                              (when orgel-gui (let ((elem (aref (slot-value (aref (orgel-gui-orgeln orgel-gui) orgelidx) slot-sym)
                                                                                faderidx)))
                                                                (unless (or (equal src elem) (not elem)) (setf (clog:value elem) val-string))))))
                                          clog-connection::*connection-data*))
 ;;; call the defined route functions
                               (dolist (fn (aref (slot-value (aref *route-responder-registry* orgelidx) slot-sym) faderidx)) ;;; call fns established with route presets.
                                 (funcall fn val))))))))
            *orgel-fader-target-syms*
            *orgel-fader-targets*)

      (let* ((orgelidx orgelidx)
             (slot-key :mlevel)
             (global-meter-ref (aref *orgel-mlevel* orgelidx))
             (orgel-name (1+ orgelidx)))
        (dotimes (faderidx 16)
          (let ((faderidx faderidx))
            (setf (set-cell-hook (aref global-meter-ref faderidx))
                  (lambda (val &key src)
                    (declare (ignorable src))
;;;                    (if val (fader-to-pd orgel-name slot-key (1+ faderidx) val))
                    (maphash
                    (lambda (key client)
                      (unless (equal key src)
  ;;                      (incudine.util:msg :info "set-cell-hook: to ~a: /~a/~a ~a" key orgel-name slot-key val)
                        (incudine.osc:message (oscout client) (format nil "/~a/~a" orgel-name slot-key) "f")  (float val 1.0)))
                    *clients*)
                    (let ((meter-value (- (val (aref (aref *orgel-mlevel* orgelidx) faderidx)) 100)))
                      (maphash (lambda (connection-id connection-hash)
                                 (declare (ignore connection-id))
                                 (let* ((orgel-gui (gethash "orgel-gui" connection-hash)))
                                   (when orgel-gui
                                     (let ((elem (elt (slot-value
                                                       (aref (orgel-gui-orgeln orgel-gui) orgelidx)
                                                       'cl-orgel-gui::meters)
                                                      faderidx)))
                                       (unless (or (equal src elem) (not elem))
                                         (setf (clog:attribute elem "data-db") meter-value))))))
                               clog-connection::*connection-data*)))))))))
  (register-cc-ref-cell-hooks)
  (register-notein-ref-cell-hooks))


;;; (setup-ref-cell-hooks)
