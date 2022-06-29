;;; functions.lisp --- Functions I find useful, that should be everywhere

;;; Commentary:
;; There are many functions that I feel should be part of a "standard library"
;; which are not. This is fine, becaues I define my own functions to behave like
;; standard library functions.
;;
;; All of the functions in this file are intended to be ones that can be used
;; from anywhere in my StumpWM configuration and are meant to be generally
;; useful.

;;; Code:

(in-package :stumpwm)


;;; Generally useful functions
(defun strip-whitespace (str)
  "String leading AND trailing whitespace from `str'."
  (assert (stringp str))
  ;; TODO: Improve this to use a well-defined (by someone else) list of whitespace
  ;; characters.
  (string-trim
      '(#\Space #\Newline #\Backspace #\Tab
        #\Linefeed #\Page #\Return #\Rubout)
      str))


;;; Notifications
(defun send-notification (summary msg)
  "Send `msg' to the user through the dunst notification daemon.

`msg' MUST be a string that can be understood by the `dunstify' command."
  (assert (stringp msg))
  (run-shell-command
   (format nil "dunstify -h ~a '~a'" summary msg)))
