(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 0)

(use-package exwm
  :ensure t)

(use-package dmenu
  :ensure t)

(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-k") #'exwm-workspace-delete)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-swap)

(exwm-input-set-key (kbd "s-SPC") #'dmenu)

(defun tassos/open-terminal ()
  "Open ansi-term with zsh in newly created window."
  (interactive)
  (ansi-term "/bin/zsh"))

(exwm-input-set-key (kbd "<s-return>") #'tassos/open-terminal)

(exwm-input-set-key (kbd "s-j") #'windmove-left)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
(exwm-input-set-key (kbd "s-i") #'windmove-up)
(exwm-input-set-key (kbd "s-k") #'windmove-down)

(require 'buffer-move)

(exwm-input-set-key (kbd "s-J") #'buf-move-left)
(exwm-input-set-key (kbd "s-L") #'buf-move-right)
(exwm-input-set-key (kbd "s-I") #'buf-move-up)
(exwm-input-set-key (kbd "s-K") #'buf-move-down)

(setq exwm-workspace-number 4)

(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'exwm-init-hook 'dashboard-refresh-buffer))

(use-package sudo-edit
  :ensure t
  :bind
  ("s-e" . sudo-edit))

(defun audio/mute ()
  (interactive)
  (shell-command "pactl set-sink-mute 0 toggle"))

(defun audio/lower-volume ()
  (interactive)
  (shell-command "volctl 3%-"))

(defun audio/raise-volume ()
  (interactive)
  (shell-command "volctl 3%+"))

(exwm-input-set-key (kbd "<XF86AudioMute>") 'audio/mute)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'audio/lower-volume)
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 'audio/raise-volume)

(exwm-enable)
