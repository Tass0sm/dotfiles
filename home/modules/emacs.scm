(define-module (home modules emacs)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  ;; from rde
  #:use-module (gnu home-services emacs))

(define emacs-packages
  (map specification->package
       '(;; Emacs-Lisp
         "emacs-use-package"
         ;; Basic Tools
         "emacs-ivy"
         "emacs-which-key"
         "emacs-projectile"
         "emacs-corfu"
         "emacs-helpful"
         "emacs-consult-dir"
         "emacs-direnv"
         "emacs-avy"
         "emacs-embark"
         "emacs-beacon"
         "emacs-unkillable-scratch"
         ;; Tool Modes
         "emacs-magit"
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
   (simple-service 'emacs-init
                   home-files-service-type
                   `(("config/emacs/early-init.el"
                      ,(local-file "../files/early-init.el"))
                     ("config/emacs/init.el"
                      ,(local-file "../files/init.el"))))
   (service home-emacs-service-type
            (home-emacs-configuration
             (package emacs-next-pgtk)
             ;; (rebuild-elisp-packages? #t)
             ;; (server-mode? #t)
             (elisp-packages emacs-packages)))))
