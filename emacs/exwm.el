(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 0)

(require 'exwm)
(exwm-enable)

(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-k") #'exwm-workspace-delete)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-swap)

(use-package dmenu
  :ensure t
  :bind
  ("s-SPC" . 'dmenu))

(setq exwm-workspace-number 4)

(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))

(exwm-input-set-key (kbd "s-j") #'windmove-left)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-i") #'windmove-up)
(exwm-input-set-key (kbd "s-k") #'windmove-down)

(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (message "hello"))
