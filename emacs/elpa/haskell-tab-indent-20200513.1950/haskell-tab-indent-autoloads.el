;;; haskell-tab-indent-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "haskell-tab-indent" "haskell-tab-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from haskell-tab-indent.el

(autoload 'haskell-tab-indent-mode "haskell-tab-indent" "\
Haskell indentation mode for projects requiring that only tabs
-- with no spaces -- be used for indentation.

If called interactively, enable Haskell-Tab-Indent mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Binds the TAB key to cycle between possible indents.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; haskell-tab-indent-autoloads.el ends here
