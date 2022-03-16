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

(define-key *root-map* (kbd "c") "exec xfce4-terminal")
(define-key *root-map* (kbd "C-c") "exec xfce4-terminal")
(define-key *root-map* (kbd "z") "exec nyxt")

;; My Personal Emacs keybindings
(defvar *emacs-keybindings*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "e") "exec emacsclient -s server -c")
    (define-key m (kbd "m") "exec emacsclient -s server -c -e (mu4e)")
    m ; NOTE: Important to return to make final value of let-binding be keymap
    ))
(define-key *root-map* (kbd "e") '*emacs-keybindings*)
