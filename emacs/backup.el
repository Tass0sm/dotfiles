(use-package ryo-modal
  :commands ryo-modal-mone
  :chords ("jk" . ryo-modal-mode)
  :bind ("C-c SPC" . ryo-modal-mode)
  :config
  (ryo-modal-keys
   ("q" ryo-modal-mode)
   ("s" isearch-forward)
   ("a" avy-goto-char-timer)
   ("j" backward-char)
   ("k" next-line)
   ("l" forward-char)
   ("i" previous-line)
   ("x" execute-extended-command)
   ("v" scroll-up-command)
   ("u" scroll-down-command)
   ("c" recenter-top-bottom))
  (ryo-modal-keys
   ("p"
    (("p" projectile-switch-project :name "Switch Project")
     ("f" projectile-find-file)))
   ("f"
    (("f" find-file :name "Find File")
     ("s" save-buffer :name "Save Buffer")
     ("k" kill-buffer :name "Kill Buffer")))
   ("o"
    (("a" org-agenda-list :name "Org Agenda List")))
   ("b"
    (("s" ivy-switch-buffer)))
   ("w"
    (("1" delete-other-windows))))
  (ryo-modal-mode 1))
