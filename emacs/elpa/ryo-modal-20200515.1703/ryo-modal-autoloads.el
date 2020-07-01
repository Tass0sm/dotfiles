;;; ryo-modal-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ryo-modal" "ryo-modal.el" (0 0 0 0))
;;; Generated autoloads from ryo-modal.el

(autoload 'ryo-modal-key "ryo-modal" "\
Bind KEY to TARGET in `ryo-modal-mode'.

TARGET can be one of:

kbd-string   Pressing KEY will simulate TARGET as a keypress.
command      Calls TARGET interactively.
list         Each element of TARGET is sent to `ryo-modal-key' again, with
             KEY as a prefix key.  ARGS are copied, except for :name.
             :name will be used by `which-key' (if installed) to name
             the prefix key, if `which-key-enable-extended-define-key'
             is t.
:hydra       If you have hydra installed, a new hydra will be created and
             bound to KEY.  The first element of ARGS should be a list
             containing the arguments sent to `defhydra'.

ARGS should be of the form [:keyword option]... if TARGET is a kbd-string
or a command.  The following keywords exist:

:name      A string, naming the binding.  If ommited get name from TARGET.
:exit      If t then exit `ryo-modal-mode' after the command.
:read      If t then prompt for a string to insert after the command.
:mode      If set to a major or minor mode symbol (e.g. 'org-mode) the key will
           only be bound in that mode.
:norepeat  If t then do not become a target of `ryo-modal-repeat'.
:then      Can be a quoted list of additional commands that will be run after
           the TARGET.  These will not be shown in the name of the binding.
           (use :name to give it a nickname).
:first     Similar to :then, but is run before the TARGET.

If any ARGS are given, except :mode and/or :norepeat, a new command named
ryo:<hash>:<name> will be created. This is to make sure the name of the created
command is unique.

\(fn KEY TARGET &rest ARGS)" nil nil)

(autoload 'ryo-modal-keys "ryo-modal" "\
Bind several keys in `ryo-modal-mode'.
Typically each element in ARGS should be of the form (key target [keywords]).
The target should not be quoted.
The first argument may be a list of keywords; they're applied to all keys:

  (:exit t :then '(kill-region)).

See `ryo-modal-key' for more information.

\(fn &rest ARGS)" nil t)

(autoload 'ryo-modal-major-mode-keys "ryo-modal" "\
Bind several keys in `ryo-modal-mode', but only if major mode is MODE.
ARGS is the same as `ryo-modal-keys'.

\(fn MODE &rest ARGS)" nil t)

(autoload 'ryo-modal-command-then-ryo "ryo-modal" "\
Define key BINDING to COMMAND in KEYMAP. Then activate `ryo-modal-mode'.
If COMMAND is excluded, use what is bound to right now in KEYMAP.
If KEYMAP is excluded, use `current-global-map'.

\(fn BINDING &optional COMMAND KEYMAP)" nil nil)

(autoload 'ryo-modal-set-key "ryo-modal" "\
Give KEY a binding as COMMAND in `ryo-modal-mode-map'.

This function is meant to be used interactively, if you want to
temporarily bind a key in ryo.

See `global-set-key' for more info.

\(fn KEY COMMAND)" t nil)

(autoload 'ryo-modal-unset-key "ryo-modal" "\
Remove `ryo-modal-mode-map' binding of KEY.
KEY is a string or vector representing a sequence of keystrokes.

This function is meant to unbind keys set with `ryo-modal-set-key'.

\(fn KEY)" t nil)

(autoload 'ryo-modal-bindings "ryo-modal" "\
Display a buffer of all bindings in `ryo-modal-mode'." t nil)

(autoload 'ryo-modal-mode "ryo-modal" "\
Toggle `ryo-modal-mode'.

If called interactively, enable ryo-Modal mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ryo-modal" '("ryo-modal-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ryo-modal-autoloads.el ends here
