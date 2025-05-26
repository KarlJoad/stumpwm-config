;;; commands.lisp --- Personal commands

;;; Commentary:
;; All commands below are adapted from the end-session contrib module.
;; That module assumes SystemD is present (which is a fair assumption), but does
;; not hold on Guix System.
;; I have modified the commands to use `loginctl' instead of `systemctl'.

;;; Code:


;;;
;;; New StumpWM command types
;;; You can use these in defcommand's interactive argument list to grab
;;; arguments from the user and have them validated before passing them to your
;;; defined function.
;;;

(defun parse-port (n)
  (multiple-value-bind (num i) (parse-integer n)
    (cond ((= i (length n))
           (if (and (>= num 0)
                    (<= num 65535))
               num
               (error 'parse-error)))
          (t (error 'parse-error)))))

(define-stumpwm-type :port-number (input prompt)
  (when-let ((n (or (argument-pop input)
                    (read-one-line (current-screen) prompt))))
    (handler-case (parse-port n)
      (parse-error (c)
        (declare (ignore c))
        (throw 'error "Ports must be integers in range 0-65535")))))

(defun yes-no-dialog (query-string)
  "Presents a yes/no dialog to the user asking QUERY-STRING.
Returns true when yes is selected."
  (equal :yes (cadr (select-from-menu (current-screen)
                                      '(("No" :no) ("Yes" :yes))
                                      query-string))))

(defun close-all-apps ()
  "Gracefully close all windows managed by StumpWM."
  ())

(defcommand (delete-marked tile-group) () ()
  "Delete all marked windows in the current group and clear the marks."
  (let ((group (current-group)))
    (dolist (i (marked-windows group))
      (delete-window i))
    (clear-window-marks group)))

;; Shutdown the computer
(defcommand shutdown-computer () ()
  (let ((choice (yes-no-dialog "Really Shutdown? (All programs will be closed)")))
    (when choice
      (echo-string (current-screen) "Shutting down...")
      ;; TODO: Find a good way to close all apps
      (close-all-apps)
      (run-hook *quit-hook*)
      (run-shell-command "loginctl poweroff"))))

;; Alias poweroff-computer to shutdown-computer
;; Many systemd-based distros allow privileged users to halt the system using
;; the privileged systemd target of poweroff, which I am used to.
(defcommand poweroff-computer () () (shutdown-computer))

(defcommand restart-computer () ()
  (let ((choice (yes-no-dialog "Really Reboot? (All programs will be closed)")))
    (when choice
      (echo-string (current-screen) "Restarting...")
      ;; TODO: Find a good way to close all apps
      (close-all-apps)
      (run-hook *quit-hook*)
      (run-shell-command "loginctl reboot"))))

(defcommand reboot-computer () ()
  (restart-computer))

(defcommand logout () ()
  (let ((choice (yes-no-dialog "Close all programs and quit StumpWM? (All programs will be closed)")))
    (when choice
      (echo-string (current-screen) "Ending session...")
      ;; TODO: Find a good way to close all apps
      (close-all-apps)
      (run-hook *quit-hook*)
      (quit))))

(defvar *end-session-menu*
  (list (list "Logout" #'logout)
        (list "Shutdown" #'shutdown-computer)
        (list "Restart" #'restart-computer))
  "The options available to quit a StumpWM session.
Entries in the list have the format of
(\"item in menu\" #'command/function-to-call)")


;;; Browser
(defcommand open-browser () ()
  (run-or-raise "firefox" '(:class "Firefox")))
