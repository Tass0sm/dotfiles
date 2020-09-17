;;; inferior-plisp.el --- Run inferior PicoLisp processes

;; Copyright (C) 2009-2019  Guillermo R. Palavecine <grpala@gmail.com>, Thorsten Jolitz <tjolitz@gmail.com>, Alexis <flexibeast@gmail.com>

;; Author: Guillermo R. Palavecine <grpala@gmail.com>
;;         Thorsten Jolitz <tjolitz@gmail.com>
;;         Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; URL: https://github.com/flexibeast/picolisp-mode
;; Keywords: picolisp, lisp, programming, org

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; A fork of tj64's `inferior-picolisp'
;; (https://github.com/tj64/picolisp-mode/), modified to be compatible
;; with `plisp-mode'
;; (https://github.com/flexibeast/plisp-mode/). Initial work on the
;; fork was done by cryptorick (https://github.com/cryptorick).

;;; Code:


(require 'comint)

;;
;; User-customisable settings.
;;

(defgroup inferior-plisp nil
  "Run a PicoLisp process in a buffer."
  :group 'picolisp)

(defcustom inferior-plisp-command-line "/usr/bin/pil +"
  "Command line for calling an inferior PicoLisp process."
  :type 'string
  :group 'inferior-plisp)

(defcustom inferior-plisp-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp are not saved on the history list.

Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'inferior-plisp)

(defcustom inferior-plisp-load-hook nil
  "Hook run when `inferior-plisp' is loaded."
  :type 'hook
  :group 'inferior-plisp)

(defcustom inferior-plisp-mode-hook nil
  "Hook for customizing `inferior-plisp'."
  :type 'hook
  :group 'inferior-plisp)

(defcustom inferior-plisp-provide-inferior-picolisp t
  "Compatibility option for `ob-picolisp'. 

When set to `t', `inferior-plisp' will register itself as
providing the `inferior-picolisp' feature required by
`ob-picolisp', and will alias the `run-picolisp' function to the
`inferior-plisp-run-picolisp' function.

Set this to `nil' to if you wish to use another package to
provide the `inferior-picolisp' feature."
  :type 'boolean
  :group 'inferior-plisp)

(defcustom inferior-plisp-source-modes '(plisp-mode picolisp-mode)
  "List of modes which indicate a buffer contains PicoLisp source code.

Used by `inferior-plisp-load-file' to determine defaults."
  :type '(repeat function)
  :group 'inferior-plisp )

(defvar inferior-plisp-picolisp-buffer nil
  "The current PicoLisp process buffer.

MULTIPLE PROCESS SUPPORT
==================================================================

`inferior-plisp' supports, in a fairly simple fashion, running
multiple PicoLisp processes. To run multiple PicoLisp processes,
you start the first up with \\[inferior-plisp-run-picolisp]. It
will be in a buffer named *picolisp*. Rename this buffer with
\\[rename-buffer]. You may now start up a new process with
another \\[inferior-plisp-run-picolisp]. It will be in a new
buffer, named *picolisp*. You can switch between the different
process buffers with \\[switch-to-buffer].

Whenever \\[inferior-plisp-run-picolisp] starts a new process, it
resets `inferior-plisp-picolisp-buffer' to be the new process'
buffer. If you only run one process, this will do the right
thing. If you run multiple processes, you can change
`inferior-plisp-picolisp-buffer' to another process buffer with
\\[set-variable].")


;;
;; Internal variables.
;;

(defvar inferior-plisp--emacs-as-editor-p nil
  "If non-nil, use `eedit.l' instead of `edit.l'.")

(defvar inferior-plisp--previous-load nil
  "Caches the last (directory . file) pair used by `inferior-plisp-load-file'.

Used for determining the default in the next call to that function.")


;;
;; Internal functions.
;;

(defun inferior-plisp--disable-line-editor ()
  "Disable inbuilt PicoLisp line-editor.

The line-editor is not needed when PicoLisp is run as an Emacs subprocess."
  (let ((pil-tmp-dir (expand-file-name "~/.pil/")))
    ;; renaming of existing editor file
    (cond
     ;; abnormal condition, something went wrong before
     ((and
       (member "editor" (directory-files pil-tmp-dir))
       (member "editor-orig" (directory-files pil-tmp-dir)))
      (let ((ed-size
             (nth
              7
              (file-attributes
               (expand-file-name "editor" pil-tmp-dir))))
            (ed-orig-size
             (nth
              7
              (file-attributes
               (expand-file-name "editor-orig"  pil-tmp-dir)))))
        (if (or (= ed-size 0)
                (<= ed-size ed-orig-size))
            (delete-file
             (expand-file-name "editor" pil-tmp-dir))
          (rename-file
           (expand-file-name "editor" pil-tmp-dir)
           (expand-file-name "editor-orig" pil-tmp-dir)
           'OK-IF-ALREADY-EXISTS))))
     ;; normal condition, only editor file exists
     ((member "editor" (directory-files pil-tmp-dir ))
      (rename-file
       (expand-file-name "editor" pil-tmp-dir)
       (expand-file-name "editor-orig" pil-tmp-dir))))
    ;; after renaming, create new empty editor file
    (with-current-buffer
        (find-file-noselect
         (expand-file-name "editor" pil-tmp-dir))
      (erase-buffer)
      (save-buffer)
      (kill-buffer))))

(defun inferior-plisp--get-editor-info ()
  "Find out if Emacs is used as editor."
  (let* ((editor-file (expand-file-name "editor" "~/.pil/"))
         (editor-orig-file (expand-file-name "editor-orig" "~/.pil/"))
         (ed-file
          (cond
           ((file-exists-p editor-file) editor-file)
           ((file-exists-p editor-orig-file) editor-orig-file)
           (t nil))))
    (when ed-file
      (with-current-buffer (find-file-noselect ed-file)
        (goto-char (point-min))
        (if (re-search-forward "eedit" nil 'NOERROR)
            (setq inferior-plisp--emacs-as-editor-p t)
          (setq inferior-plisp--emacs-as-editor-p nil))
        (kill-buffer)))))

(defun inferior-plisp--get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun inferior-plisp--input-filter (str)
  "Don't save anything matching `inferior-plisp-filter-regexp'."
  (not (string-match inferior-plisp-filter-regexp str)) )

(defun inferior-plisp--picolisp-process ()
  "Return the current PicoLisp process, starting one if necessary.

See variable `inferior-plisp-picolisp-buffer'."
  (unless (and inferior-plisp-picolisp-buffer
               (get-buffer inferior-plisp-picolisp-buffer)
               (comint-check-proc inferior-plisp-picolisp-buffer))
    (inferior-plisp-interactively-start-process))
  (or (get-buffer-process
       (if (eq major-mode 'inferior-plisp-mode)
           (current-buffer)
         inferior-plisp-picolisp-buffer))
      (error "No current process. See `inferior-plisp-picolisp-buffer'")))

(defun inferior-plisp--reset-line-editor ()
  "Reset inbuilt PicoLisp line-editor to original state."
  (let ((pil-tmp-dir (expand-file-name "~/.pil/")))
    (if (member "editor-orig" (directory-files pil-tmp-dir))
        (rename-file
         (expand-file-name "editor-orig" pil-tmp-dir)
         (expand-file-name "editor" pil-tmp-dir)
         'OK-IF-ALREADY-EXISTS)
      (delete-file
       (expand-file-name "editor" pil-tmp-dir)))))


;;
;; User-facing functions.
;;

(defun inferior-plisp-interactively-start-process (&optional cmd)
  "Start an inferior PicoLisp process.  Return the process started.

Since this command is run implicitly, always ask the user for the
command to run."
  (save-window-excursion
    (inferior-plisp-run-picolisp
     (read-string "Run PicoLisp: " inferior-plisp-command-line))))

(defun inferior-plisp-load-file (file-name)
  "Load PicoLisp file FILE-NAME into the inferior PicoLisp process."
  (interactive
   (comint-get-source  "Load PicoLisp file: "
                       inferior-plisp--previous-load
                       inferior-plisp-source-modes
                       t)) ; `t' to indicate the file must already exist
  (comint-check-source file-name)
  (setq inferior-plisp--previous-load
        (cons (file-name-directory file-name)
              (file-name-nondirectory file-name)))
  (comint-send-string
   (inferior-plisp--picolisp-process) (concat "(load \"" file-name "\")\n")))

;;;###autoload
(defun inferior-plisp-run-picolisp (cmd)
  "Run an inferior PicoLisp process, input and output via buffer `*picolisp*'.

If there is a process already running in `*picolisp*', switch to
that buffer.

With argument, allows you to edit the command line; default is value
of `inferior-plisp-command-line'.

Runs the hook `inferior-plisp-mode-hook' (after the `comint-mode-hook'
is run)."

  (interactive (list
                (if current-prefix-arg
                    (read-string "Run PicoLisp: "
                                 inferior-plisp-command-line)
                  inferior-plisp-command-line)))
  (message "Using `run-picolisp' from `inferior-plisp'.")
  (when (not (comint-check-proc "*picolisp*"))
    (let ((cmdlist (split-string cmd)))
      (inferior-plisp--get-editor-info)
      (inferior-plisp--disable-line-editor)
      (set-buffer
       (apply 'make-comint
              "picolisp"
              (car cmdlist)
              nil
              ;; hack for multi-word PicoLisp arguments:
              ;; separate them with '_XXX_' in the 'cmd' arg
              ;; instead of blanks
              (mapcar
               (lambda (--arg)
                 (replace-regexp-in-string
                  "_XXX_" " " --arg))
               (if inferior-plisp--emacs-as-editor-p
                   (cons "@lib/eedit.l" (cdr cmdlist))
                 (cons "@lib/edit.l" (cdr cmdlist))))))
      (inferior-plisp--reset-line-editor)
      (inferior-plisp-mode)))
  (setq inferior-plisp-command-line cmd)
  (setq inferior-plisp-picolisp-buffer "*picolisp*")
  (pop-to-buffer "*picolisp*"))

(defun inferior-plisp-send-definition ()
  "Send the current definition to the inferior PicoLisp process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (inferior-plisp-send-region
       (point) (progn (forward-sexp) (point))))))

(defun inferior-plisp-send-definition-and-go ()
  "Send the current definition to the inferior PicoLisp,
then switch to the process buffer."
  (interactive)
  (inferior-plisp-send-definition)
  (inferior-plisp-switch-to-picolisp t))

(defun inferior-plisp-send-last-sexp ()
  "Send the previous sexp to the inferior PicoLisp process."
  (interactive)
  (inferior-plisp-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun inferior-plisp-send-region (start end)
  "Send the current region to the inferior PicoLisp process."
  (interactive "r")
  (let ((region-substring
         (replace-regexp-in-string "^\n"
                                   ""
                                   (buffer-substring start end))))
    (progn
      (comint-send-string
       (inferior-plisp--picolisp-process)
       (if (string= "" (car (last (split-string region-substring "\n"))))
           region-substring
         (concat region-substring "\n"))))))

(defun inferior-plisp-send-region-and-go (start end)
  "Send the current region to the inferior PicoLisp process,
then switch to the process buffer."
  (interactive "r")
  (inferior-plisp-send-region start end)
  (inferior-plisp-switch-to-picolisp t))


;;;###autoload
(defun inferior-plisp-support-ob-picolisp ()
  "Enable Org Babel session support.

Needs `inferior-picolisp-provide-inferior-picolisp' set to `t'."
  (interactive)
  (if inferior-plisp-provide-inferior-picolisp
      (progn
        ;; We use `quote' here for Emacsmirror:
        ;; https://github.com/flexibeast/plisp-mode/pull/13
        (provide (quote inferior-picolisp))
        (defalias 'run-picolisp 'inferior-plisp-run-picolisp))
    (error "Unable to support ob-picolisp: please ensure 'inferior-plisp-provide-inferior-picolisp' is set to 't'")))

(defun inferior-plisp-switch-to-picolisp (eob-p)
  "Switch to the *picolisp* process buffer.

With argument, position cursor at end of buffer."
  (interactive "P")
  (if (or (and inferior-plisp-picolisp-buffer
               (get-buffer inferior-plisp-picolisp-buffer))
          (inferior-plisp-interactively-start-process) )
      (pop-to-buffer inferior-plisp-picolisp-buffer)
    (error "No current process buffer. See `inferior-plisp-picolisp-buffer'") )
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

;;;###autoload (add-hook 'same-window-buffer-names "*picolisp*")
(define-derived-mode inferior-plisp-mode comint-mode "Inferior PicoLisp"
  "Major mode for interacting with an inferior PicoLisp process.

A PicoLisp process can be started with 'M-x inferior-plisp-run-picolisp'.

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inferior-plisp-mode-hook' (in that order).

For information on running multiple processes in multiple buffers, see
documentation for the variable `inferior-plisp-picolisp-buffer'."
  ;; The following can be customised via `inferior-plisp-mode-hook'.
  (setq comint-prompt-regexp "^[^\n:?!]*[?!:]+ *")
  (setq comint-prompt-read-only nil)
  (setq comint-input-filter (function inferior-plisp--input-filter))
  (setq comint-get-old-input (function inferior-plisp--get-old-input))
  (setq mode-line-process '(":%s"))
  (setq comint-input-ring-file-name "~/.pil_history"))


;; --

(inferior-plisp-support-ob-picolisp)
(run-hooks 'inferior-plisp-load-hook)
(provide 'inferior-plisp)

;;; inferior-plisp.el ends here
