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

(defun get-ifname-and-ip (s)
  "get the interface name and its ip address for the first interface name
starting with \"en*\" or \"eth*\". As the function uses the ip and the
cut command in a shell, those programs need to be installed on the
system."
  (with-input-from-string
      (in
       (string-right-trim
        '(#\NEWLINE)
        (uiop:run-program
         "ip -4 -o a | cut -d ' ' -f 2,7 | cut -d '/' -f 1"
         :output :string)))
    (loop
      for line = (read-line in nil nil)
      while line
      for if-ip = (remove "" (uiop:split-string line) :test #'string=) 
      until (and (> (length (first if-ip)) 1) (string= (subseq (first if-ip) 0 1) s))
      finally (return (if line if-ip)))))

;;; (get-ifname-and-ip)

(defun create-slynk-server ()
  (destructuring-bind (&optional interface host &rest rest)
      (or (get-ifname-and-ip "e")
          (get-ifname-and-ip "w"))
    (declare (ignore rest))
    (sleep 0.5)
    (let* ((port 4007)
           (str (format nil "~&~%using interface ~a~%creating server on host ~a, port ~a~%"
                        interface host port)))
      (if host
          (progn
            (slynk:create-server :interface host :port port :dont-close t)
            (format t str))
          (warn "no ethernet interface found")))
    (setf slynk::*use-dedicated-output-stream* nil)
    host))

;;; (create-slynk-server)

;;; (ql:quickload "cl-orgelctl-server")

#|
(let* ((interface "enp6s0f3u1u3c2")
       (host
         (string-right-trim
          '(#\NEWLINE)
          (uiop:run-program
           (format nil "ip -f inet addr show ~a | grep inet | awk '{print $2}' | sed -e 's/\\/[0-9]*$//g'"
                   interface)
           :output :string))))
  (slynk:create-server :interface host :port 4007 :dont-close t))
|#
