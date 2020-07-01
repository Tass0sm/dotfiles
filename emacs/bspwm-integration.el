(setq initial-frame (selected-frame))
(setq initial-window (selected-window))
(setq last-buffer (window-buffer (selected-window)))

(defun printf (&rest args)
  (princ (apply #'format args) #'external-debugging-output))

(defun update-last-buffer ()
  "Update last-buffer to the current-buffer, ignoring the
internal daemon frame"
  (if (not (eq (selected-window) initial-window))
      (setq last-buffer (window-buffer (selected-window)))
    (printf "Closed The Last Window.\n"))
  (printf "HOOK RAN, Buffer: %s, Window: %s, Frame: %s\n" (current-buffer) (selected-window) (selected-frame)))

(add-hook 'buffer-list-update-hook  'update-last-buffer)

(defun current-buffer+ ()
  "The most recently open buffer accross all clients. Immune to
the internal daemon frame."
  (interactive)
  last-buffer)

(defun current-directory ()
  "The directory of the currently visited file, or nil if not
visiting a file."
  (let ((current-file (buffer-file-name (window-buffer (selected-window)))))
    (if current-file
	(if (not (directory-name-p current-file))
	    (cl-loop for i downfrom (- (length current-file) 1) to 0
		     until (equal (substring current-file i (+ i 1)) "/")
		     finally return (substring current-file 0 i))))))

;;(setq global-current-buffer (get-buffer-create "*scratch*"))
;;(setq test-count 0)



;;
;;(defun on-internal ()
;;  (if (eq (selected-frame) initial-frame)
;;      (printf "On Internal")))
;;
;;(defun print-error-to-stdout-normal ()
;;  (if (not (eq (selected-frame) initial-frame))
;;      (printf "Hook ran with buffer %s in frame %s\n" (current-buffer) (selected-frame))))
;;
;;(add-hook 'buffer-list-update-hook  'on-internal)
;;(add-hook 'buffer-list-update-hook  'print-error-to-stdout-normal)
;;
;;

;;(defun test-balls ()
;;  (interactive)
;;  "Balls"
;;  (message "BALLS!"))
;;
;;(defun update-global-buffer ()
;;  "Update the global-current-buffer variable to buffer."
;;  (interactive)
;;  (message "balls")
;;  (setq test-count (+ test-count 1))
;;  (if (eq (selected-frame) initial-frame)
;;      (message "closed the frame into the initial frame")
;;    (message "ok this is fine")
;;    (setq global-current-buffer (current-buffer))
;;    (message "%s" global-current-buffer)))
;;

;;(add-hook 'focus-out-hook  'print-error-to-stdout)
;;
;;;;(add-hook 'buffer-list-update-hook 'update-global-buffer)
;;
;;
;;
;;(defun set-global-buffer (buffer-or-name &optional norecord force-same-window)
;;  "Update the global-current-buffer variable to buffer-or-name."
;;  (setq global-current-buffer buffer-or-name))
;;
;;(defun set-global-buffer2 (window buffer-or-name &optional keep-margins)
;;  "Update the global-current-buffer variable to buffer-or-name."
;;  (setq global-current-buffer buffer-or-name))
;;

;;(advice-add 'forward-char :after 'test-current-buffer)
;;(advice-add 'switch-to-buffer :after 'set-global-buffer)
;;(advice-add 'set-window-buffer :after 'set-global-buffer2)


;;
;;(defun set-global-current-buffer (buffer-or-name &optional action frame)
;;  (interactive)
;;  "Set the global-current-buffer variable to buffer-or-name."
;;  (setq global-current-buffer buffer-or-name))
;;  
;;(advice-add 'display-buffer :after 'set-global-current-buffer)
;;(add-hook 'window-selection-change-functions 'update-global-buffer)
;;
;;
;;
;;(defun set-global-buffer3 (buffer alist)
;;  "Update the global-current-buffer variable to buffer-or-name."
;;  (setq global-current-buffer buffer))

;;(setq display-buffer-alist '(("*" . ('set-global-buffer3 . ()))))

;;(defun test-current-buffer ()
;;  (interactive)
;;  "Interactive test function to get read the current buffer
;;anywhere."
;;  (message "Buffer: %s" (current-buffer)))
;;(advice-add 'set-buffer :after 'test-balls)
