;;; borg-mode.el --- instant universal documentation
;;
;; Author: Tassos Manganaris

;;; Commentary:
;;
;; borg-mode.el is part of my effort to get documentation (like with Emacs's
;; C-h o command) for every language, library, program, and more.
;;
;; See included README.md for more information.
;;
;;; Code:

;; borg help

(defun borg-documented-symbol-list ()
  "Return the list of symbols to pick from when searching for
documentation."
  '("test-function" "test-variable" "test-macro"))

(defun borg-document-thing (thing)
  "Search for the documentation of the thing named THING. If not
documented, return nil."
  (if (zerop (call-process "borg" nil (get-buffer-create "tmp-buffer") nil "define" thing))
      (save-excursion
        (let (result)
          (set-buffer "tmp-buffer")
          (setq result (buffer-string)) ; This seems bad. Make buffer, go to
                                        ; buffer, copy string, return.
          (kill-buffer "tmp-buffer")
          result))
    nil))

;; main functions

(defun borg-describe-thing (thing)
  "Display the full documentation of FUNC (a symbol) via borg."
  (interactive
   (let* ((fn (ivy-thing-at-point))
          (enable-recursive-minibuffers t)
          (val (completing-read
                (if fn
                    (format "Describe function (default %s): " fn)
                  "Describe function: ")
                (borg-documented-symbol-list)
                (lambda (f) t)
                t
                nil
                nil
                fn)))
     (if (equal val "")
         (user-error "You didn't specify a function symbol")
       (setq fn val))
     (unless (borg-document-thing fn)
       (user-error "%s's documentation wasn't found" fn))
     (list fn))) ; (Return the list of values to use for arguments to the function)

  ;; Start body (setup references for help buffer, create or change display of
  ;; help buffer)

  (let ((describe-function-orig-buffer (current-buffer)))

    ;; setup references for help buffer

    (help-setup-xref
     (list (lambda (function buffer)
             (let ((describe-function-orig-buffer (if (buffer-live-p buffer)
                                                      buffer)))
               (describe-function function)))
           thing
           describe-function-orig-buffer)
     (called-interactively-p 'interactive))

    ;; start excursion to the help buffer

    (save-excursion
      (with-help-window (help-buffer)
        (princ thing)
        (princ ":\n\n")
        (princ (borg-document-thing thing))))))
