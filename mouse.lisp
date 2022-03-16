;;; mouse.lisp --- Making the mouse more usable

;;; Code:

;; Make the frame that is currently focused the one that JUST received a click event
(setf *mouse-focus-policy* :click)

;; StumpWM uses the X11 standard X cursor. Switch to usual left-facing pointer.
;; This requires the xsetroot package actually be available to your user.
(run-shell-command "xsetroot -cursor_name left_ptr")
