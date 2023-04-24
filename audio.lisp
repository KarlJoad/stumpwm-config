;;; audio.lisp --- Audio management and configuration

;;; Code:

(in-package :stumpwm)

(defconstant +karljoad/dunst-volume-app-name+ "Volume"
  "Dunst appname to use for for volume notifications.")

(defconstant +karljoad/dunst-volume-hint+ "string:x-dunst-stack-tag:volume"
  "Dunst tag/hint to use for volume notifications.

Previous notifications that use the same tag/hint will automatically and
immediately be replaced by the newer notification. This can be useful for in
certain situations, such as presenting volume change feedback without delay.")

(defconstant +karljoad/dunst-mute-hint+ "string:x-dunst-stack-tag:mute"
  "Dunst tag/hint to use for mute notifications.

Previous notifications that use the same tag/hint will automatically and
immediately be replaced by the newer notification. This can be useful for in
certain situations, such as presenting volume change feedback without delay.")

(defconstant +karljoad/dunst-volume-timeout+ 1000
  "Amount of time dunst volume notification should live, in milliseconds.")

(defun karljoad/volume-notification (volume-level)
  "Send a notification tagged with the `+karljoad/dunst-volume-hit+' tag to dunst
with a message containing VOLUME-LEVEL."
  (send-notification
   +karljoad/dunst-volume-app-name+
   +karljoad/dunst-volume-hint+
   (format nil "Current Volume: ~a" volume-level)
   +karljoad/dunst-volume-timeout+))

(defun karljoad/mute-notification (is-muted?)
  "Send a notification to dunst with the mute tag."
  (send-notification
   +karljoad/dunst-volume-app-name+
   +karljoad/dunst-mute-hint+
   (format nil "~a" (string-capitalize (if is-muted? "Muted" "Unmuted")))
   +karljoad/dunst-volume-timeout+))

(defun karljoad/current-volume  ()
  "Get the current volume level, as reported by `amixer'."
  (strip-whitespace
   (run-shell-command "amixer get Master | grep '%' | head -n 1 | cut -d '[' -f 2 | cut -d '%' -f 1" t)))

(defun karljoad/is-muted? ()
  "Return Boolean value representing if the current audio output if muted."
  (string-equal
   (strip-whitespace
    (run-shell-command "amixer get Master | grep '%' | head -n 1 | cut -d '[' -f 3 | cut -d ']' -f 1" t))
   "off"))

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

(defun karljoad/control-song (dest action)
  "Send an MPRIS dbus message to the media player at DEST to perform ACTION.

ACTION should be a playback control method name, such as PlayPause, Next,
Previous, and others provided as a string.
You can see the full list of action methods at
https://specifications.freedesktop.org/mpris-spec/latest/Player_Interface.html"
  (dbus-send 't 'session dest "/org/mpris/MediaPlayer2"
             (concat "org.mpris.MediaPlayer2.Player." action)))

(defcommand karljoad/play-pause-song () ()
  "Toggle the play/pause state of the currently playing track in Spotify."
  (karljoad/control-song "org.mpris.MediaPlayer2.spotify" "PlayPause"))

(defcommand karljoad/next-song () ()
  "Move to the next track in Spotify."
  (karljoad/control-song "org.mpris.MediaPlayer2.spotify" "Next"))

(defcommand karljoad/prev-song () ()
  "Move to the previous track in Spotify."
  (karljoad/control-song "org.mpris.MediaPlayer2.spotify" "Previous"))
