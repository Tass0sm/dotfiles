(setq initial-frame (selected-frame))
(setq initial-window (selected-window))
(setq real-current-buffer (current-buffer))
(setq real-current-directory default-directory)

(defun printf (&rest args)
  (princ (apply #'format args) #'external-debugging-output))

(defun actually-selected-frame ()
  "Return the selected frame as known by frame-focus-state." 
  (car (cl-remove-if-not 'frame-focus-state (visible-frame-list))))

(defun update-current-buffer+ (&optional one two)
  "Update real-current-buffer to the current-buffer, ignoring the
internal daemon frame"
  (setq real-current-buffer (window-buffer
			     (frame-selected-window
			      (actually-selected-frame)))))

(defun update-current-directory+ (&optional one two)
  "Update real-current-buffer to the current-buffer, ignoring the
internal daemon frame"
  (setq real-current-directory (expand-file-name default-directory)))

(advice-add 'select-window :after 'update-current-buffer+)
(advice-add 'select-window :after 'update-current-directory+)
(add-function :after after-focus-change-function 'update-current-buffer+)
(add-function :after after-focus-change-function 'update-current-directory+)


(defun current-buffer+ ()
  "The most recently open buffer accross all clients. Immune to
the internal daemon frame."
  (interactive)
  real-current-buffer)

(defun current-directory ()
  "The directory of the currently visited file, or nil if not
visiting a file."
  real-current-directory)
