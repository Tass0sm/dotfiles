(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

(load-file (expand-file-name
            (cond ((eq system-type 'windows-nt) "windows.el")
                  ((eq system-type 'gnu/linux) "linux.el")
                  (t "default-system.el"))
            user-emacs-directory))

