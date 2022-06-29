;;; audio.lisp --- Audio management and configuration

;;; Code:

(in-package :stumpwm)

(defconstant +karljoad/dunst-volume-app-name+ "Volume"
  "Dunst appname to use for for volume notifications.")

(defconstant +karljoad/dunst-volume-hint+ "string:x-dunst-stack-tag:volume"
  "Dunst tag/hint to use for volume notifications.")

(defconstant +karljoad/dunst-volume-timeout+ 1000
  "Amount of time dunst volume notification should live, in milliseconds.")

(defun karljoad/volume-notification (msg)
  "Send a notification to dunst with the volume tag."
  (send-notification
   +karljoad/dunst-volume-app-name+
   +karljoad/dunst-volume-hint+
   (format nil "Current Volume: ~a" msg)
   +karljoad/dunst-volume-timeout+))

(defun karljoad/current-volume  ()
  "Get the current volume level, as reported by `amixer'."
  (strip-whitespace
   (run-shell-command "amixer get Master | grep '%' | head -n 1 | cut -d '[' -f 2 | cut -d '%' -f 1" t)))

(defcommand karljoad/lower-volume (amount)
    ((:number "Amount to lower volume (in %): "))
  "Lower the volume by `AMOUNT' amount and notify the user of the new current volume."
  (run-shell-command
   (concat "exec amixer -q set Master " (write-to-string amount) "%- unmute"))
  (karljoad/volume-notification (karljoad/current-volume)))

(defcommand karljoad/raise-volume (amount)
    ((:number "Amount to raise volume (in %): "))
  "Raise the volume by `AMOUNT' amount and notify the user of the new current volume."
  (progn
    (run-shell-command
     (concat "exec amixer -q set Master " (write-to-string amount) "%+ unmute"))
    (karljoad/volume-notification (karljoad/current-volume))))
