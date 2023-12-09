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

(defun get-interface-ip (line)
  "get the interface name and its ip address as strings from a line like
\"lo 127.0.0.1\""
  (let ((split-point (position #\SPACE line)))
    (list (subseq line 0 split-point)
          (string-trim '(#\SPACE) (subseq line split-point)))))

(defun get-ifname-and-ip ()
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
      for if-ip = (get-interface-ip line) 
      until (and (> (length (first if-ip)) 2)
                 (or (string= (subseq (first if-ip) 0 2) "en")
                     (string= (subseq (first if-ip) 0 3) "eth")))
      finally (return (if line if-ip)))))

(destructuring-bind (&optional interface host)
    (get-ifname-and-ip)
  (sleep 0.5)
  (let* ((port 4007)
         (str (format nil "~&~%using interface ~a~%creating server on host ~a, port ~a~%"
                      interface host port)))
    (if host
        (progn
          (slynk:create-server :interface host :port port :dont-close t)
          (format t str))
        (warn "no ethernet interface found"))))


;;; (slynk:create-server :interface host :port 4007 :dont-close t)

;;; (slynk:create-server :port 4007 :dont-close t)
(setf slynk*use-dedicated-output-stream* nil)


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





