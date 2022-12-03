;;; 
;;; preset-parser.lisp
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

(defparameter *orgel-preset-def-lookup* (make-hash-table))

(dolist (target *orgel-fader-targets*)
  (dotimes (i 16)
    (setf (gethash (read-from-string (format nil ":~a~2,'0d" target (1+ i))) *orgel-preset-def-lookup*)
          (list 'orgel-ctl target (1+ i)))))

(dolist (target *orgel-global-targets*)
  (setf (gethash target *orgel-preset-def-lookup*)
        (list 'orgel-ctl-global target)))
