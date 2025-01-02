;;; macros.lisp --- A "standard library" of macros for StumpWM

;;; Commentary:
;; All of the functions in this file are intended to be ones that can be used
;; from anywhere in my StumpWM configuration and are meant to be generally
;; useful.

;;; Code:

(in-package :stumpwm)

(defmacro search-all-run-or-raise (cmd-name app-bin prog-name window-props)
  (declare (type symbol cmd-name)
           (type string app-bin prog-name))
  "Create a StumpWM command for a messaging application.

Creates a StumpWM command named CMD-NAME for a messaging application that
searches all groups and screens matching against the provided WINDOW-PROPS and
running `stumpwm:run-or-raise' using APP-BIN.

CMD-NAME is a symbol that should NOT be quoted. The symbol used must be
compatible with StumpWM's `stumpwm:defcommand'.

PROG-NAME is used in the command's documentation string.

NOTE: CMD-NAME should NOT be quoted!

Example:

\(messaging-app cmd-name \"app-bin\" \"app-name\"
  '(:class \"app-class\" :instance \"app-instance\"))

The properties for a program's window can be found with
`stumpwm:show-window-properties'."
  `(defcommand ,cmd-name () ()
     ,(uiop:strcat "Run or raise " prog-name ".")
     (sb-thread:make-thread
      (lambda ()
        ;; Search all groups and all screens
        (run-or-raise ,app-bin ,window-props t t)))))
