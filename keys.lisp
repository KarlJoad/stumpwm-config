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

(define-key *root-map* (kbd "c") "exec alacritty")
(define-key *root-map* (kbd "C-c") "exec alacritty")
(define-key *root-map* (kbd "u") "exec urxvt")
(define-key *root-map* (kbd "z") "exec nyxt")

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

;; Set up media control keys
;; From https://config.phundrak.com/stumpwm.html#Keybinds-Media-and-Media-Control-hbv5uk91z5j0
(define-interactive-keymap audio-interactive nil
  ((kbd "XF86AudioLowerVolume") "exec amixer -q set Master 5%- unmute")
  ((kbd "XF86AudioRaiseVolume") "exec amixer -q set Master 5%+ unmute"))
  ;; ((kbd "XF86AudioMute") "exec amixer -q set Master 1+ toggle"))

;; TODO: Give feedback after running command.
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec amixer -q set Master 5%- unmute")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec amixer -q set Master 5%+ unmute")

;; (defvar *media-keymap*
;;   (let ((m (make-sparse-keymap)))
;;     (define-key

;; Add a way to lock the screen.
(define-key *top-map* (kbd "s-l") "exec xlock -mode blank")
