;;; 
;;; cl-orgel-gui-redefs.lisp
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

(in-package :cl-orgel-gui)

(defun make-orgel-attr-val-receiver (slot orgelidx global-orgel-ref &key (attribute "data-val"))
  (let ((slot-symbol (intern (format nil "~:@(~a~)" slot) 'cl-orgel-gui)))
    (lambda (val self)
      (let* ((val-string (ensure-string val))
             (orgel-val (/ (read-from-string val-string)
                           (if (member slot '(:main :bias-bw)) 100.0 1.0)))
;;;             (num-val (read-from-string val-string))
             )
        (setf (slot-value global-orgel-ref slot-symbol) orgel-val)
        (cl-orgelctl::orgel-ctl (cl-orgelctl::orgel-name (1+ orgelidx)) slot orgel-val)
        (maphash (lambda (connection-id connection-hash)
                   (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                   (let* ((orgel-gui (gethash "orgel-gui" connection-hash)))
                     (when orgel-gui (let ((elem (slot-value (aref (orgel-gui-orgeln orgel-gui) orgelidx) slot-symbol)))
;;;                                       (break "self: ~a~% elem: ~a" self elem)
                                       (unless (equal self elem) (setf (attribute elem attribute) val-string))))))
                 clog-connection::*connection-data*)))))

(defun make-orgel-val-receiver (slot orgelidx global-orgel-ref)
  (let ((slot-symbol (intern (format nil "~:@(~a~)" slot) 'cl-orgel-gui)))
    (lambda (val self)
      (let* ((val-string (ensure-string val))
;;;             (num-val (read-from-string val-string))
             (orgel-val (/ (read-from-string val-string)
                           (if (member slot '(:main :bias-pos :bias-bw)) 100.0 1.0))))
;;;        (break "val-receiver: ~S" slot)
        (setf (slot-value global-orgel-ref slot-symbol) orgel-val)
        (cl-orgelctl::orgel-ctl (cl-orgelctl::orgel-name (1+ orgelidx)) slot orgel-val)
        (maphash (lambda (connection-id connection-hash)
                   (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                   (let* ((orgel-gui (gethash "orgel-gui" connection-hash)))
                     (when orgel-gui (let ((elem (slot-value (aref (orgel-gui-orgeln orgel-gui) orgelidx) slot-symbol)))
;;;                                       (break "self: ~a~% elem: ~a" self elem)
                                       (unless (equal self elem) (setf (value elem) val-string))))))
                 clog-connection::*connection-data*)))))

(defun make-orgel-array-receiver (slot orgelidx global-orgel-ref)
  (let ((g-accessor (slot->function "g-orgel" slot))
        (accessor (slot->function "orgel" slot)))
    (lambda (idx val self)
      (let* ((val-string (ensure-string val))
;;;             (num-val (read-from-string val-string))
             (orgel-val (/ (read-from-string val-string) 100.0))
             )
        (setf (aref (funcall accessor global-orgel-ref) idx) orgel-val)
        (cl-orgelctl::orgel-ctl-fader (cl-orgelctl::orgel-name (1+ orgelidx)) slot (1+ idx) orgel-val)
        (maphash (lambda (connection-id connection-hash)
                   (declare (ignore connection-id))
;;;                   (break "~a" (gethash "orgel-gui" connection-hash))
                   (let* ((orgel-gui (gethash "orgel-gui" connection-hash))
                          (orgel (aref (orgel-gui-orgeln orgel-gui) orgelidx)))
                     (when orgel-gui (let ((elem (aref (funcall g-accessor orgel) idx)))
;;;                                       (break "~a" orgel)
                                       (unless (equal self elem) (setf (value elem) val-string))))))
                 clog-connection::*connection-data*)))))
