(in-package :cl-user)

(ql:quickload :cl-orgelctl-server)

(cl-orgelctl::start-orgel-server :local-host "192.168.7.21")
