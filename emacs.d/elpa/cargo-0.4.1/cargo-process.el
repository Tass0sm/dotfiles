;;; cargo-process.el --- Cargo Process Major Mode -*-lexical-binding: t-*-

;; Copyright (C) 2015  Kevin W. van Rooijen

;; Author: Kevin W. van Rooijen <kevin.van.rooijen@attichacker.com>
;; Keywords: processes, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Cargo Process Major mode.
;; Used to run Cargo background processes.
;; Current supported Cargo functions:
;;  * cargo-process-bench              - Run the benchmarks.
;;  * cargo-process-build              - Compile the current project.
;;  * cargo-process-clean              - Remove the target directory.
;;  * cargo-process-doc                - Build this project's and its dependencies' documentation.
;;  * cargo-process-doc-open           - Open this project's documentation.
;;  * cargo-process-new                - Create a new cargo project.
;;  * cargo-process-init               - Create a new cargo project inside an existing directory.
;;  * cargo-process-run                - Build and execute src/main.rs.
;;  * cargo-process-run-example        - Build and execute with --example <name>.
;;  * cargo-process-run-bin            - Build and execute a specific binary.
;;  * cargo-process-search             - Search registry for crates.
;;  * cargo-process-test               - Run all unit tests.
;;  * cargo-process-update             - Update dependencies listed in Cargo.lock.
;;  * cargo-process-repeat             - Run the last cargo-process command.
;;  * cargo-process-current-test       - Run the current unit test.
;;  * cargo-process-current-file-tests - Run the current file unit tests.
;;  * cargo-process-fmt                - Run the optional cargo command fmt.
;;  * cargo-process-check              - Run the optional cargo command check.
;;  * cargo-process-clippy             - Run the optional cargo command clippy.

;;
;;; Code:

(require 'compile)
(require 'button)
(require 'rust-mode)

(defgroup cargo-process nil
  "Cargo Process group."
  :prefix "cargo-process-"
  :group 'cargo)

(defcustom cargo-process--custom-path-to-bin nil
  "Custom path to the directory containing the cargo executable"
  :type 'directory
  :group 'cargo-process)

(defvar cargo-process-mode-map
  (nconc (make-sparse-keymap) compilation-mode-map)
  "Keymap for Cargo major mode.")

(defvar cargo-process-last-command nil "Command used last for repeating.")

(defface cargo-process--ok-face
  '((t (:foreground "#00ff00")))
  "Ok face"
  :group 'cargo-process)

(defface cargo-process--error-face
  '((t (:foreground "#FF0000")))
  "Error face"
  :group 'cargo-process)

(defface cargo-process--warning-face
  '((t (:foreground "#eeee00")))
  "Warning face"
  :group 'cargo-process)

(defface cargo-process--pointer-face
  '((t (:foreground "#ff00ff")))
  "Pointer face"
  :group 'cargo-process)

(defface cargo-process--standard-face
  '((t (:foreground "#ffa500")))
  "Standard face"
  :group 'cargo-process)

(defface cargo-process--errno-face
  '((t :foreground "#7777ff"
       :underline t))
  "Error number face"
  :group 'cargo-process)

(defconst cargo-process-test-regexp "^[[:space:]]*fn[[:space:]]*"
  "Regex to find Rust unit test function.")

(defconst cargo-process-font-lock-keywords
  '(("error\\:" . 'cargo-process--error-face)
    ("warning\\:" . 'cargo-process--warning-face)
    ("^\s*\\^\\~*\s*$" . 'cargo-process--pointer-face)
    ("^\s*Compiling.*" . 'cargo-process--standard-face)
    ("^\s*Running.*" . 'cargo-process--standard-face)
    ("^\s*Updating.*" . 'cargo-process--standard-face)
    ("test result: FAILED." . 'cargo-process--error-face)
    ("test result: ok." . 'cargo-process--ok-face)
    ("test\s.*\sFAILED" . 'cargo-process--error-face)
    ("test\s.*\sok" . 'cargo-process--ok-face))
  "Minimal highlighting expressions for cargo-process mode.")

;; Bind `case-fold-search' to nil before using the regex.
(defconst cargo-process--errno-regex "\\bE[0-9]\\{4\\}\\b"
  "A regular expression to match Rust error number.")

(define-button-type 'rustc-errno
  'follow-link t
  'face 'cargo-process--errno-face
  'action #'cargo-process--explain-action)

(defun cargo-process--project-root ()
  "Find the root of the current Cargo project."
  (let ((root (locate-dominating-file (or buffer-file-name default-directory) "Cargo.toml")))
    (and root (file-truename root))))

(define-derived-mode cargo-process-mode compilation-mode "Cargo-Process."
  "Major mode for the Cargo process buffer."
  (use-local-map cargo-process-mode-map)
  (setq major-mode 'cargo-process-mode)
  (setq mode-name "Cargo-Process")
  (setq-local truncate-lines t)
  (run-hooks 'cargo-process-mode-hook)
  (add-hook 'compilation-filter-hook #'cargo-process--add-errno-buttons)
  (font-lock-add-keywords nil cargo-process-font-lock-keywords))

(defun cargo-process--finished-sentinel (process event)
  "Execute after PROCESS return and EVENT is 'finished'."
  (compilation-sentinel process event)
  (when (equal event "finished\n")
    (message "Cargo Process finished.")))

(defun cargo-process--activate-mode (buffer)
  "Execute commands BUFFER at process start."
  (with-current-buffer buffer
    (funcall 'cargo-process-mode)
    (setq-local window-point-insertion-type t)))

(defun cargo-process--start (name command)
  "Start the Cargo process NAME with the cargo command COMMAND."
  (let* ((buffer (concat "*Cargo " name "*"))
         (path cargo-process--custom-path-to-bin)
         (path (and path (file-name-as-directory path)))
         (command (cargo-process--maybe-read-command (concat path command)))
         (project-root (cargo-process--project-root))
         (default-directory (or project-root default-directory)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (and project-root
                              buffer-file-name
                              (string-prefix-p project-root (file-truename buffer-file-name)))))
    (setq cargo-process-last-command (list name command))
    (compilation-start command 'cargo-process-mode (lambda(_) buffer))
    (set-process-sentinel (get-buffer-process buffer) 'cargo-process--finished-sentinel)))

(defun cargo-process--explain-action (button)
  "Action called when the user activates Rust errno BUTTON."
  (cargo-process--explain-help (button-label button)))

(defun cargo-process--explain-help (errno)
  "Display a detailed explaination of ERRNO in the Help buffer."
  (help-setup-xref (list #'cargo-process--explain-help errno)
                   (called-interactively-p 'interactive))
  (save-excursion
    (with-help-window (help-buffer)
      (princ (shell-command-to-string
              (concat "rustc --explain=" errno)))
      (with-current-buffer standard-output
        (buffer-string)))))

(defun cargo-process--add-errno-buttons ()
  "Turn error numbers into clickable links in Cargo process output.
Meant to be run as a `compilation-filter-hook'."
  (save-excursion
    (let ((start compilation-filter-start)
          (end (point))
          (case-fold-search nil))
     (goto-char start)
     (while (re-search-forward cargo-process--errno-regex end t)
       (make-button (match-beginning 0)
                    (match-end 0)
                    :type 'rustc-errno)))))

(defun cargo-process--get-current-test ()
  "Return the current test."
  (let ((start (point)))
    (save-excursion
      (end-of-line)
      (unless (and (search-backward-regexp cargo-process-test-regexp nil t)
                   (save-excursion (rust-end-of-defun) (< start (point))))
        (error "Unable to find test"))
      (search-forward "fn ")
      (thing-at-point 'sexp))))

(defun cargo-process--maybe-read-command (default)
  "Prompt to modify the DEFAULT command when the prefix argument is present.
Without the prefix argument, return DEFAULT immediately."
  (if current-prefix-arg
      (read-shell-command "Cargo command: " default)
    default))

;;;###autoload
(defun cargo-process-bench ()
  "Run the Cargo bench command.
With the prefix argument, modify the command's invocation.
Cargo: Run the benchmarks."
  (interactive)
  (cargo-process--start "Bench" "cargo bench"))

;;;###autoload
(defun cargo-process-build ()
  "Run the Cargo build command.
With the prefix argument, modify the command's invocation.
Cargo: Compile the current project."
  (interactive)
  (cargo-process--start "Build" "cargo build"))

;;;###autoload
(defun cargo-process-clean ()
  "Run the Cargo clean command.
With the prefix argument, modify the command's invocation.
Cargo: Remove the target directory."
  (interactive)
  (cargo-process--start "Clean" "cargo clean"))

;;;###autoload
(defun cargo-process-doc ()
  "Run the Cargo doc command.
With the prefix argument, modify the command's invocation.
Cargo: Build this project's and its dependencies' documentation."
  (interactive)
  (cargo-process--start "Doc" "cargo doc"))

;;;###autoload
(defun cargo-process-doc-open ()
  "Run the Cargo doc command with the --open switch.
With the prefix argument, modify the command's invocation.
Cargo: Open this project's documentation."
  (interactive)
  (cargo-process--start "Doc" "cargo doc --open"))

;;;###autoload
(defun cargo-process-new (name &optional bin)
  "Run the Cargo new command.
With the prefix argument, modify the command's invocation.
NAME is the name of your application.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project."
  (interactive "sProject name: ")
  (let ((bin (when (or bin (y-or-n-p "Create Bin Project? ")) " --bin")))
    (cargo-process--start "New" (concat "cargo new " name bin))))

;;;###autoload
(defun cargo-process-init (directory &optional bin)
  "Run the Cargo init command.
With the prefix argument, modify the command's invocation.
DIRECTORY is the directory you want to create a cargo project in.
If BIN is t then create a binary application, otherwise a library.
Cargo: Create a new cargo project in current directory."
  (interactive
   (list (read-directory-name "Directory: " nil default-directory t)))
  (let ((bin (when (or bin (y-or-n-p "Create Bin Project? ")) " --bin")))
    (cargo-process--start "Init" (concat "cargo init " directory bin))))

;;;###autoload
(defun cargo-process-run ()
  "Run the Cargo run command.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute src/main.rs."
  (interactive)
  (cargo-process--start "Run" "cargo run"))

;;;###autoload
(defun cargo-process-run-bin (command)
  "Run the Cargo run command --bin <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute a specific binary"
  (interactive "sBinary name: ")
  (cargo-process--start (concat "Run " command)
                        (concat "cargo run --bin " command)))

;;;###autoload
(defun cargo-process-run-example (command)
  "Run the Cargo run command --example <name>.
With the prefix argument, modify the command's invocation.
Cargo: Build and execute with --example <name>."
  (interactive "sExample name: ")
  (cargo-process--start (concat "Example " command)
                        (concat "cargo run --example " command)))

;;;###autoload
(defun cargo-process-search (search-term)
  "Run the Cargo search command.
With the prefix argument, modify the command's invocation.
SEARCH-TERM is used as the search term for the Cargo registry.
Cargo: Search registry for crates."
  (interactive "sSearch: ")
  (cargo-process--start "Search" (concat "cargo search " search-term)))

;;;###autoload
(defun cargo-process-test ()
  "Run the Cargo test command.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests."
  (interactive)
  (cargo-process--start "Test" "cargo test"))

;;;###autoload
(defun cargo-process-current-test ()
  "Run the Cargo test command for the current test.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests."
  (interactive)
  (cargo-process--start "Test" (format "cargo test %s::%s"
                                       (file-name-base)
                                       (cargo-process--get-current-test))))

;;;###autoload
(defun cargo-process-current-file-tests ()
  "Run the Cargo test command for the current file.
With the prefix argument, modify the command's invocation.
Cargo: Run the tests."
  (interactive)
  (cargo-process--start "Test" (concat "cargo test " (file-name-base))))

;;;###autoload
(defun cargo-process-update ()
  "Run the Cargo update command.
With the prefix argument, modify the command's invocation.
Cargo: Update dependencies listed in Cargo.lock."
  (interactive)
  (cargo-process--start "Update" "cargo update"))

;;;###autoload
(defun cargo-process-fmt ()
  "Run the Cargo fmt command.
With the prefix argument, modify the command's invocation.
Requires Cargo Fmt to be installed."
  (interactive)
  (cargo-process--start "Fmt" "cargo fmt"))

;;;###autoload
(defun cargo-process-check ()
  "Run the Cargo check command.
With the prefix argument, modify the command's invocation.
Cargo: Check compile the current project.
Requires cargo-check to be installed."
  (interactive)
  (cargo-process--start "Check" "cargo check"))

;;;###autoload
(defun cargo-process-clippy ()
  "Run the Cargo clippy command.
With the prefix argument, modify the command's invocation.
Cargo: Clippy compile the current project.
Requires Cargo clippy to be installed."
  (interactive)
  (cargo-process--start "Clippy" "cargo clippy"))

;;;###autoload
(defun cargo-process-repeat ()
  "Run the last cargo-process command."
  (interactive)
  (if cargo-process-last-command
      (apply 'cargo-process--start cargo-process-last-command)
    (message "No last Cargo command.")))

(define-key cargo-process-mode-map (kbd "n") 'forward-button)
(define-key cargo-process-mode-map (kbd "p") 'backward-button)

(provide 'cargo-process)
;;; cargo-process.el ends here
