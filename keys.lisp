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
(set-prefix-key (kbd "C-z"))

;; Show the keymap box as I go.
(which-key-mode)

;; Add keybinds for floating group creation.
(define-key *groups-map* (kbd "f") "gnew-float")
(define-key *groups-map* (kbd "F") "gnewbg-float")
(define-key *groups-map* (kbd "G") "gselect")

;; My Personal Emacs keybindings
(defvar *emacs-keybindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "e") "exec emacsclient -s server -c")
    (define-key m (kbd "d") "exec emacsclient -s debug -c")
    (define-key m (kbd "m") "exec emacsclient -s server -c -e '(mu4e)'")
    (define-key m (kbd "E") "exec emacs")
    m ; NOTE: Important to return to make final value of let-binding be keymap
    ))
(define-key *root-map* (kbd "e") '*emacs-keybindings*)

(defvar *window-keybindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "n") "renumber")
    (define-key m (kbd "p") "pull-window-by-number")
    m ; NOTE: Important to return to make final value of let-binding be keymap
    ))
(define-key *root-map* (kbd "w") '*window-keybindings*)

;; Set up media control keys
;; From https://config.phundrak.com/stumpwm.html#Keybinds-Media-and-Media-Control-hbv5uk91z5j0
(define-interactive-keymap audio-interactive nil
  ;; The raise/lower volume functions will ASK for the amount to raise/lower by.
  ((kbd "XF86AudioLowerVolume") "karljoad/lower-volume")
  ((kbd "XF86AudioRaiseVolume") "karljoad/raise-volume"))
  ;; ((kbd "XF86AudioMute") "exec amixer -q set Master 1+ toggle"))

;; TODO: Give feedback after running command.
(define-key *top-map* (kbd "XF86AudioLowerVolume") "karljoad/lower-volume 5")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "karljoad/raise-volume 5")

;; (defvar *media-keymap*
;;   (let ((m (make-sparse-keymap)))
;;     (define-key


;;; Major keybindings to add to *top-map*, which is reached by pressing the prefix-key
;; Add a way to lock the screen.
(define-key *top-map* (kbd "s-l") "exec xlock -mode blank")
(define-key *top-map* (kbd "s-f") "exec firefox")
(define-key *top-map* (kbd "s-e") "exec thunar")
;; Terminal Emulators
(define-key *root-map* (kbd "c") "exec alacritty")
(define-key *root-map* (kbd "C-c") "exec alacritty")
(define-key *root-map* (kbd "u") "exec urxvt")
(define-key *root-map* (kbd "z") "exec nyxt")
