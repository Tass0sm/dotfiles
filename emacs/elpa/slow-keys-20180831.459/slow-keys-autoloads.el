;;; slow-keys-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "slow-keys" "slow-keys.el" (0 0 0 0))
;;; Generated autoloads from slow-keys.el

(defvar slow-keys-mode nil "\
Non-nil if Slow-Keys mode is enabled.
See the `slow-keys-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `slow-keys-mode'.")

(custom-autoload 'slow-keys-mode "slow-keys" nil)

(autoload 'slow-keys-mode "slow-keys" "\
Type slowly to avoid RSI.

If called interactively, enable Slow-Keys mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slow-keys" '("slow-keys-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; slow-keys-autoloads.el ends here
