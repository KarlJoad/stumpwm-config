;;; keys.lisp --- Keybinding configurations

;;; Code:

(in-package :stumpwm)

;; TODO: Figure out how my KBD Hyper (H) key is understood by StumpWM
;; StumpWM uses the following shorthands for:
;; C - Control
;; M - Meta (Left Alt)
;; H - Hyper
;; s - Super (Windows Key)
;; (set-prefix-key (kbd "C-M-
;; (set-prefix-key (kbd "s-z"))
(set-prefix-key (kbd "C-z"))

;; Show the keymap box as I go.
(which-key-mode)

 
(define-key *groups-map* (kbd "C") "gnewbg")
;;; Add keybinds for floating group creation.
(define-key *groups-map* (kbd "f") "gnew-float")
(define-key *groups-map* (kbd "F") "gnewbg-float")
;;; Add keybinds for dynamic group creation.
(define-key *groups-map* (kbd "d") "gnew-dynamic")
(define-key *groups-map* (kbd "D") "gnewbg-dynamic")

;; Use g in *groups-map* to select the group to switch to, by substring
(define-key *groups-map* (kbd "g") "gselect")
;; Use G in *groups-map* to list all the currently open groups
(define-key *groups-map* (kbd "G") "groups")
;; Use l in *groups-map* to switch to last-open group, the same way "l" works
;; as an "Alt-Tab" for windows in the *root-map*.
(define-key *groups-map* (kbd "l") "gother")


;;; My Personal keybindings
;; Emacs
(defvar *emacs-keybindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "e") "exec emacsclient -s server -c")
    (define-key m (kbd "d") "exec emacsclient -s debug -c")
    (define-key m (kbd "m") "exec emacsclient -s server -c -e '(mu4e)'")
    (define-key m (kbd "E") "exec emacs")
    m ; NOTE: Important to return to make final value of let-binding be keymap
    ))
(define-key *root-map* (kbd "e") '*emacs-keybindings*)

;; Window repositioning keybindings
(defvar *window-keybindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "n") "renumber")
    (define-key m (kbd "p") "pull-window-by-number")
    (define-key m (kbd "p") "repack-window-numbers")
    (define-key m (kbd "w") "windowlist")
    (define-key m (kbd "o") "other-window")
    (define-key m (kbd "h") "move-focus left")
    (define-key m (kbd "j") "move-focus down")
    (define-key m (kbd "k") "move-focus up")
    (define-key m (kbd "l") "move-focus right")
    m ; NOTE: Important to return to make final value of let-binding be keymap
    ))
(define-key *root-map* (kbd "w") '*window-keybindings*)

;; Media control keys
;; From https://config.phundrak.com/stumpwm.html#Keybinds-Media-and-Media-Control-hbv5uk91z5j0
(defvar *audio-interactive*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "l") "karljoad/lower-volume 5")
    (define-key m (kbd "r") "karljoad/raise-volume 5")
    ;; The raise/lower volume functions will ASK for the amount to raise/lower by.
    (define-key m (kbd "L") "karljoad/lower-volume")
    (define-key m (kbd "R") "karljoad/raise-volume")
    (define-key m (kbd "m") "karljoad/toggle-mute")
    (define-key m (kbd "SPC") "karljoad/play-pause-song")
    (define-key m (kbd "n") "karljoad/next-song")
    (define-key m (kbd "p") "karljoad/prev-song")
    m))
(define-key *root-map* (kbd "m") '*audio-interactive*)

(defprogram-shortcut spotify
  :command "spotify"
  :props '(:class "Spotify" :instance "spotify")
  :map *audio-interactive*
  :key (kbd "s")
  :pullp nil)

;; Monitor brightness control keys
(define-interactive-keymap brightness-interactive nil
  ((kbd "XF86MonBrightnessDown") "karljoad/lower-brightness")
  ((kbd "XF86MonBrightnessUp") "karljoad/raise-brightness"))

(define-key *top-map* (kbd "XF86AudioLowerVolume") "karljoad/lower-volume 5")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "karljoad/raise-volume 5")
(define-key *top-map* (kbd "XF86AudioMute") "karljoad/toggle-mute")
(define-key *top-map* (kbd "XF86AudioPlay") "karljoad/play-pause-song")
(define-key *top-map* (kbd "XF86AudioNext") "karljoad/next-song")
(define-key *top-map* (kbd "XF86AudioPrev") "karljoad/prev-song")

(define-key *top-map* (kbd "XF86MonBrightnessDown") "karljoad/lower-brightness 5")
(define-key *top-map* (kbd "XF86MonBrightnessUp") "karljoad/raise-brightness 5")

(defvar *messaging-keymap* (make-sparse-keymap))
(define-key *root-map* (kbd "C-m") '*messaging-keymap*)

(defprogram-shortcut slack
  :command "slack"
  :props '(:class "Slack" :instance "slack")
  :map *messaging-keymap*
  :key (kbd "s")
  :pullp nil)

(defprogram-shortcut discord
  :command "Discord"
  :props '(:class "discord" :instance "discord")
  :map *messaging-keymap*
  :key (kbd "d")
  :pullp nil)

(defprogram-shortcut telegram
  :command "telegram-desktop"
  :props '(:class "TelegramDesktop" :instance "telegram-desktop")
  :map *messaging-keymap*
  :key (kbd "t")
  :pullp nil)

(defprogram-shortcut element
  :command "element-desktop"
  :props '(:class "Element" :instance "element")
  :map *messaging-keymap*
  :key (kbd "e")
  :pullp nil)

(defprogram-shortcut zoom
  :command "ZoomLauncher"
  :props '(:class "zoom" :instance "zoom")
  :map *messaging-keymap*
  :key (kbd "Z")
  :pullp nil)

(defprogram-shortcut zulip
  :command "zulip"
  :props '(:class "Zulip" :instance "zulip")
  :map *messaging-keymap*
  :key (kbd "z")
  :pullp nil)

;; If we use *prefix-key* C-q, let the next keybinding be sent to the selected
;; frame. This mirrors the kind of behavior Emacs has.
(define-key *root-map* (kbd "C-q") "send-raw-key")


;;; Major keybindings to add to *top-map*, which is the map that *prefix-key*
;;; gets you to. The keybindings below do not need to be led with the
;;; *prefix-key* combination.
;; Add a way to lock the screen.
(define-key *top-map* (kbd "s-l") "exec xlock -mode blank")
(define-key *top-map* (kbd "s-f") "open-browser")
(define-key *top-map* (kbd "s-e") "exec dolphin")


;;; Keybindings added to *root-map*, which is reached by pressing *prefix-key*.
;; Terminal Emulators
(define-key *root-map* (kbd "c") "exec alacritty")
(define-key *root-map* (kbd "C-c") "exec alacritty")
(define-key *root-map* (kbd "u") "exec urxvt")
(define-key *root-map* (kbd "z") "exec nyxt")
(define-key *root-map* (kbd "C-o") "other-window")

;;; Window movement
;; This will give us behavior similar to Alt-Tab in other desktop environments.
;; In this case, the keybinding was taken from tmux's default bindings. The
;; mnemonic to remember is "last-window", not "other-window", as *prefix-key*-o
;; goes to the next Stump frame.
(define-key *root-map* (kbd "l") "other-window")
