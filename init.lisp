;;; init.lisp --- Starting point for StumpWM configuration

;;; Commentary:
;;
;; This config is augmented by using the `slynk' CL package. This should be
;; installed by either Quicklisp, or potentially through Emacs.

;;; Code:
(in-package :stumpwm)

(defvar karljoad/display-number
  (multiple-value-bind (_ array)
      (cl-ppcre:scan-to-strings ":([0-9]+)" (getenv "DISPLAY"))
    (declare (ignore _))
    (if (vectorp array)
        (parse-integer (aref array 0))
      0))
  "The number of the current DISPLAY.")

;; (slynk:create-server
;;  :dont-close t
;;  :port (+ slynk::default-server-port karljoad/display-numer))

;; Make loading additional files easier

(defvar karljoad/init-directory
  (directory-namestring
   (truename (merge-pathnames (user-homedir-pathname)
                              ".stumpwm.d")))
  "Directory with StumpWM initialization files.")

(defun karljoad/load (filename)
  "Load Common Lisp FILENAME (without `.lisp' extension) from `karljoad/init-directory'."
  (let ((file (merge-pathnames (concat filename ".lisp")
                               karljoad/init-directory)))
    (if (probe-file file)
        (load file)
        (format *error-output* "StumpWM Config File '~a' does not exist." file))))

;; Load my personal "standard library" of functions
(karljoad/load "functions")

(karljoad/load "slynk")
(karljoad/load "mouse")
(karljoad/load "visual")
(karljoad/load "audio")
(karljoad/load "commands")
(karljoad/load "keys")

(defun karljoad/mode-line-all-heads ()
  "Turn on StumpWM's mode-line on all heads (monitors)."
  (dolist (h (screen-heads (current-screen)))
    (enable-mode-line (current-screen) h t)))

(karljoad/mode-line-all-heads)
