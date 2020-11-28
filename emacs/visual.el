(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 0)

(setq-default truncate-lines t)
(setq-default fill-column 80)

(defun clip (val max min)
  (cond ((< max val) max)
        ((> min val) min)
        (t val)))

(defun center-text-horizontally ()
  "Center the buffer in the frame horizontally, caring for the
fill-column value."
  (interactive)
  (let* ((win-width (window-total-width))
         (max-margin (/ win-width 4))
         (margin-perc (clip
                       (/ (- win-width fill-column)
                          (- (* fill-column 4.0) fill-column))
                       1 0))
         (left-margin (* margin-perc max-margin))
         (right-margin (- win-width left-margin fill-column)))
    (set-window-margins (selected-window) (round left-margin))))

(add-hook 'window-configuration-change-hook 'center-text-horizontally)

(set-face-attribute 'mode-line nil :box nil)
