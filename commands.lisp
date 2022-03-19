;;; commands.lisp --- Personal commands

;;; Commentary:
;; All commands below are adapted from the end-session contrib module.
;; That module assumes SystemD is present (which is a fair assumption), but does
;; not hold on Guix System.
;; I have modified the commands to use `loginctl' instead of `systemctl'.

;;; Code:

(defun yes-no-dialog (query-string)
  "Presents a yes/no dialog to the user asking QUERY-STRING.
Returns true when yes is selected."
  (equal :yes (cadr (select-from-menu (current-screen)
                                      '(("No" :no) ("Yes" :yes))
                                      query-string))))

(defun close-all-apps ()
  "Gracefully close all windows managed by StumpWM."
  ())

;; Shutdown the computer
(defcommand shutdown-computer () ()
  (let ((choice (yes-no-dialog "Really Shutdown? (All programs will be closed)")))
    (when choice
      (echo-string (current-screen) "Shutting down...")
      ;; TODO: Find a good way to close all apps
      (close-all-apps)
      (run-hook *quit-hook*)
      (run-shell-command "loginctl poweroff"))))

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
Entries in the list have the format of (\"item in menu\" #'function-to-call)")
