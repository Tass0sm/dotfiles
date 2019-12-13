;;; rust-playground-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rust-playground" "rust-playground.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rust-playground.el

(autoload 'rust-playground "rust-playground" "\
Run playground for Rust language in a new buffer.

\(fn)" t nil)

(autoload 'rust-playground-rm "rust-playground" "\
Remove files of the current snippet together with directory of this snippet.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-playground" '("rust-playground-" "in-rust-playground")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rust-playground-autoloads.el ends here
