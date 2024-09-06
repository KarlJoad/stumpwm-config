;;; slynk.lisp --- Interacting with StumpWM through Emacs & Sly/Slynk

;;; Commentary:
;; This configuration file is intended to be used with the `stumpwm-with-slynk'
;; Guix package. That particular spin of StumpWM compiles Sly/Slynk INTO the
;; StumpWM image, so no external file/module loading is required.

;;; Code:

(in-package :stumpwm)

;; I do NOT need these quicklisp quickload function calls, because I am using
;; a Guix package where Sly/Slynk is compiled into the StumpWM image.
;; (ql:quickload "slynk")
;; (ql:quickload :slynk)

;; This only works if sbcl is the Common Lisp implementation in use
(defcommand start-slynk (port) ((:port-number "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (slynk:create-server :port port
                          :dont-close t))
   :name "slynk-manual"))
;; TODO: Find way to stop slynk server
;; (slynk:stop-server port-number)
