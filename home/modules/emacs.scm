(define-module (home modules emacs)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (tassos-guix packages emacs)
  #:use-module (tassos-guix packages emacs-xyz)
  ;; from rde
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services-utils))

(define emacs-packages
  (map specification->package
       '(;; Emacs-Lisp
         "emacs-use-package"
         ;; Basic Tools
         "emacs-vertico"
         "emacs-orderless"
         "emacs-marginalia"
         "emacs-consult"
         "emacs-embark"
         "emacs-corfu"
         "emacs-cape"
         "emacs-which-key"
         "emacs-projectile"
         "emacs-popper"
         "emacs-consult-dir"
         "emacs-direnv"
         "emacs-avy"
         "emacs-embark"
         "emacs-unkillable-scratch"
         "emacs-wgrep"
         "emacs-bufler"
         "emacs-jupyter"
         "emacs-frames-only-mode"
         ;; Tool Modes
         "emacs-magit"
         "emacs-magit-todos"
         "emacs-guix"
         "emacs-vterm"
         "emacs-pdf-tools"
         "emacs-notmuch"
         ;; Specific Editing Modes
         "emacs-markdown-mode"
         "emacs-ledger-mode"
         "emacs-yaml-mode"
         "emacs-racket-mode"
         "emacs-geiser"
         "emacs-geiser-guile"
         "emacs-geiser-racket"
         "emacs-haskell-mode"
         "emacs-dante"
         "emacs-clojure-mode"
         "emacs-cider"
         "emacs-rust-mode"
         "emacs-macrostep"
         "emacs-web-mode"
         "emacs-js2-mode"
         "emacs-typescript-mode"
         "emacs-add-node-modules-path"
         "emacs-prettier"
         "emacs-graphviz-dot-mode"
         ;; General Editing Modes
         "emacs-sudo-edit"
         "emacs-expand-region"
         "emacs-multiple-cursors"
         "emacs-phi-search"
         "emacs-ws-butler"
         "emacs-yasnippet"
         "emacs-yasnippet-snippets"
         "emacs-ivy-yasnippet"
         "emacs-flyspell-correct"
         ;; Org
         "emacs-org-journal"
         "emacs-org-roam"
         "emacs-org-download"
         "emacs-org-contrib"
         "emacs-org-fragtog"
         ;; Appearance
         "emacs-nord-theme"
         "emacs-olivetti")))

(define-public emacs-services
  (list
   (service home-emacs-service-type
            (home-emacs-configuration
             (package emacs-next-pgtk-treesitter)
             ;; (rebuild-elisp-packages? #t)
             (elisp-packages emacs-packages)
             (init-el
              (list (slurp-file-gexp (local-file "../files/init.el"))))
             (early-init-el
              (list (slurp-file-gexp (local-file "../files/early-init.el"))))))))
