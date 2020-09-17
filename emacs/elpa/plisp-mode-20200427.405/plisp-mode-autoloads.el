;;; plisp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "inferior-plisp" "inferior-plisp.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from inferior-plisp.el

(autoload 'inferior-plisp-run-picolisp "inferior-plisp" "\
Run an inferior PicoLisp process, input and output via buffer `*picolisp*'.

If there is a process already running in `*picolisp*', switch to
that buffer.

With argument, allows you to edit the command line; default is value
of `inferior-plisp-command-line'.

Runs the hook `inferior-plisp-mode-hook' (after the `comint-mode-hook'
is run).

\(fn CMD)" t nil)

(autoload 'inferior-plisp-support-ob-picolisp "inferior-plisp" "\
Enable Org Babel session support.

Needs `inferior-picolisp-provide-inferior-picolisp' set to `t'." t nil)
 (add-hook 'same-window-buffer-names "*picolisp*")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "inferior-plisp" '("inferior-plisp-")))

;;;***

;;;### (autoloads nil "plisp-mode" "plisp-mode.el" (0 0 0 0))
;;; Generated autoloads from plisp-mode.el

(autoload 'plisp-mode "plisp-mode" "\
Major mode for PicoLisp programming. Derived from lisp-mode.

\\{plisp-mode-map}

\(fn)" t nil)

(autoload 'plisp-repl-mode "plisp-mode" "\
Major mode for `pil' REPL sessions. Derived from comint-mode.

\\{plisp-repl-mode-map}

\(fn)" t nil)

(autoload 'plisp-repl "plisp-mode" "\
Start a `pil' session in a new `plisp-repl-mode' buffer." t nil)

(autoload 'plisp-support-ob-picolisp "plisp-mode" "\
Enable editing of Org Babel PicoLisp source blocks with `plisp-mode'.

Needs `plisp-provide-picolisp-mode' set to `t'." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "plisp-mode" '("plisp-")))

;;;***

;;;### (autoloads nil nil ("plisp-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; plisp-mode-autoloads.el ends here
