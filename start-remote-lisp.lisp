;;; 
;;; start-remote-lisp.lisp
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

(in-package :cl-user)

(ql:quickload :slynk)

(let* ((interface "enp6s0f3u1u3c2")
       (host
         (string-right-trim
          '(#\NEWLINE)
          (uiop:run-program
           (format nil "ip -f inet addr show ~a | grep inet | awk '{print $2}' | sed -e 's/\\/[0-9]*$//g'"
                   interface)
           :output :string))))
  (slynk:create-server :interface host :port 4007 :dont-close t))

;;; (slynk:create-server :port 4007 :dont-close t)
(setf slynk*use-dedicated-output-stream* nil)
