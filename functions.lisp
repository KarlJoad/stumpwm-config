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
(defun send-notification (app-name hint msg &optional (timeout 1000))
  "Send `msg' to the user through the dunst notification daemon from the
application named `app-name' and a dunst hint of `hint'. The timeout for the
notification (which is in milliseconds) can be customized with the `timeout'
argument, which defaults to 1000 milliseconds.

`app-name', `hint', and `msg' MUST be a string and `timeout' must be an integer
that can be understood by the `dunstify' command."
  (assert (and (stringp app-name)
               (stringp hint)
               (stringp msg)
               (integerp timeout)))
  (run-shell-command
   (format nil "dunstify -a '~a' -h ~a -t ~a '~a'" app-name hint timeout msg)))
