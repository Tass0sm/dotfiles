(defun insert-problem ()
  "Insert problem at the end of the buffer."
  (interactive)
  (goto-char (point-max))
  (insert "\n"
          (number-to-string (random operand-limit)) " + "
	  (number-to-string (random operand-limit)) " = "))

(defun validate--problem ()
  "Check problem on current line."
  (let ((problem nil)
	(answer nil)
	(result nil))
    (save-excursion
      (search-backward "=")
      (backward-char)
      (setq problem (buffer-substring (point-at-bol) (point-marker))))
    (save-excursion
      (search-backward "=")
      (forward-char 2)
      (setq answer (string-to-number (buffer-substring (point-marker) (point-at-eol)))))
    (setq result (string-to-number (calc-eval problem)))
    (if (eq result answer)
	(insert "\nCORRECT")
      (insert "\nINCORRECT"))))

(defun complete-problem ()
  "Check current line's problem and proceed to the next."
  (interactive)
  (validate--problem)
  (insert-problem))

(defvar operand-limit 10)

(defun set-operand-limit (limit)
  "Set maximum value for problem operands"
  (interactive "nPositive operand limit: ")
  (setq operand-limit limit))

(defun mental--math-mode-setup ()
  "Function to set up minimal mode."
  (kill-all-local-variables)
  (insert-problem)
  (setq major-mode 'mental-math-mode)
  (setq mode-name "mental-math")
  (use-local-map mental-math-mode-map)
  (run-hooks 'mental-math-mode-hook))

(defun mental-math-mode ()
  "Major mode for practicing mental computation.
Special commands:
  \\{mental-math-mode-map}"
  (interactive)
  (mental--math-mode-setup))

(defvar mental-math-mode-map nil
  "Keymap for mental math mode.")

(if mental-math-mode-map
    nil
  (setq mental-math-mode-map (make-keymap))
  (define-key mental-math-mode-map "\C-c\C-n" 'insert-problem)
  (define-key mental-math-mode-map (kbd "<return>") 'complete-problem)
  (define-key mental-math-mode-map "\C-c\C-r" 'set-operand-limit))

(provide 'mental-math-mode)
