;;; visual.lisp --- Configuration for visual effects in StumpWM

;;; Commentary:
;; Enables, toggles, and sets various visual pieces of information on the screen.
;;
;; Sets my color theme to use as well.
;; The color theme is based of off Prot's modus operandi/vivendi themes, which
;; make it a requirement to conform to the highest standard for color contrast
;; between any given combination of foreground and background values.
;; This corresponds to the WCAG AAA Standard, which specifies a minimum rate of
;; distance, in relative luminance, of 7:1.
;; The modus-operandi clone is a light theme, and vivendi is a dark theme.
;; A side benefit of this color requirement is that this theme is useful for
;; those with red-green color deficiency (deuteranopia).

;;; Code:

(in-package :stumpwm)

;;; Colors

;; Most colors drawn from modus-vivendi's intense foreground colors
(setf *colors*
      '("#000000" ; 0, Black
        "#fe6060" ; 1, Red
        "#4fe42f" ; 2, Green
        "#f1e00a" ; 3, Yellow
        "#2fafff" ; 4, Blue
        "#ff62d4" ; 5, Magenta
        "#3fdfd0" ; 6, Cyan
        "#ffffff" ; 7, White
        "#b0d6f5" ;; Color 8
        "#aa9e9f" ;; Color 9
        ))
(update-color-map (current-screen))

(defmacro karljoad/set-color (val color)
  "Similar to `set-any-color', but without updating colors."
  `(dolist (s *screen-list*)
     (setf (,val s) (alloc-color s ,color))))

(karljoad/set-color screen-fg-color (hex-to-xlib-color "#ffffff"))
(karljoad/set-color screen-bg-color (hex-to-xlib-color "#000000"))
(karljoad/set-color screen-focus-color (hex-to-xlib-color "#f4f4f4"))
(karljoad/set-color screen-unfocus-color (hex-to-xlib-color "#bfc0c4"))
(karljoad/set-color screen-border-color (hex-to-xlib-color "#00d3d0"))
(karljoad/set-color screen-float-focus-color (hex-to-xlib-color "#bfc0c4"))
(karljoad/set-color screen-float-unfocus-color (hex-to-xlib-color "#aa9e9f"))

(setf *mode-line-foreground-color* (hex-to-xlib-color "#f4f4f4"))
(setf *mode-line-background-color* (hex-to-xlib-color "#323232"))
(setf *mode-line-border-color* (hex-to-xlib-color "#a8a8a8"))

(update-colors-all-screens)


;;; Modeline information

;; Set the name for each window (which inhabit a frame in a group)
(setf *window-format* "%m%n%s%c")

;; Set the mode line's format to include the window names on the left and the
;; date/time on the right.
(setf *screen-mode-line-format* (list "[^B%n^b] %W^>%d"))

;; Set the time format to be displayed in the modeline
(setf *time-modeline-string* "^B^2%a %b %e^n ^B^30%k:%M^n")

;; How often to update the modeline. Modeline is ALSO updated when a StumpWM
;; action occurs.
(setf *mode-line-timeout* 2)

;; Give the mode-line an xlib WM_CLASS property, so xprop understands the mode-line
(add-hook *new-mode-line-hook*
          (lambda (mode-line)
            (xlib:set-wm-class (mode-line-window mode-line)
                               "stumpwm-mode-line"
                               "stumpwm-mode-line")))
(xlib:set-wm-class (screen-message-window (current-screen))
                   "stumpwm-message"
                   "stumpwm-message")

;; Lastly, enable the modeline
(toggle-mode-line (current-screen) (current-head))

;; TODO: Get battery information to work properly
;; (load-module "battery-portable")


;;; Screen brightness controls.
;;; This only makes sense on laptops
(defconstant +karljoad/dunst-brightness-app-name+ "Brightness"
  "Dunst appname to use for for brightness notifications.")

(defconstant +karljoad/dunst-brightness-hint+ "string:x-dunst-stack-tag:brightness"
  "Dunst tag/hint to use for brightness notifications.

Previous notifications that use the same tag/hint will automatically and
immediately be replaced by the newer notification. This can be useful for in
certain situations, such as presenting brightness change feedback without delay.")

(defconstant +karljoad/dunst-brightness-timeout+ 1000
  "Amount of time dunst brightness notification should live, in milliseconds.")

