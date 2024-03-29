(define-module (home modules emacs)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (tassos-guix packages emacs)
  #:use-module (tassos-guix packages emacs-xyz)
  ;; from tassos-guix
  #:use-module (tassos-guix packages emacs-xyz)
  ;; from nongnu
  #:use-module (nongnu packages emacs)
  ;; from rde
  #:use-module (rde gexp)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services-utils))

(define emacs-packages
  (list
   ;; Emacs-Lisp
   emacs-use-package
   ;; Basic Tools
   emacs-vertico
   emacs-orderless
   emacs-prescient
   emacs-marginalia
   emacs-consult
   emacs-embark
   emacs-corfu
   emacs-cape
   emacs-which-key
   emacs-popper
   emacs-consult-dir
   emacs-envrc
   emacs-avy
   emacs-embark
   emacs-unkillable-scratch
   emacs-wgrep
   emacs-bufler
   emacs-jupyter-next
   emacs-frames-only-mode
   emacs-citar
   emacs-citar-org-roam
   ;; Tool Modes
   emacs-magit
   emacs-magit-todos
   emacs-guix
   emacs-vterm
   emacs-pdf-tools
   emacs-notmuch
   emacs-elfeed
   emacs-helpful
   emacs-project-x
   ;; Specific Editing Modes
   emacs-markdown-mode
   emacs-ledger-mode
   emacs-yaml-mode
   emacs-racket-mode
   emacs-geiser
   emacs-geiser-guile
   emacs-geiser-racket
   emacs-haskell-mode
   emacs-dante
   emacs-clojure-mode
   emacs-cider
   emacs-sly
   emacs-rust-mode
   emacs-macrostep
   emacs-web-mode
   emacs-js2-mode
   emacs-typescript-mode
   emacs-add-node-modules-path
   emacs-prettier
   emacs-gdscript-mode
   emacs-graphviz-dot-mode
   emacs-glsl-mode
   emacs-nim-mode
   emacs-cmake-mode
   ;; General Editing Modes
   emacs-sudo-edit
   emacs-expand-region
   emacs-multiple-cursors
   emacs-phi-search
   emacs-ws-butler
   emacs-tempel
   emacs-yasnippet
   emacs-yasnippet-snippets
   ;; emacs-jinx
   ;; Org
   emacs-org-ql
   emacs-org-alert
   emacs-org-analyzer
   emacs-org-roam
   emacs-org-roam-ui
   emacs-org-download
   emacs-org-fragtog
   emacs-org-re-reveal
   emacs-ox-haunt-latest
   emacs-engrave-faces
   ;; Appearance
   emacs-nord-theme
   emacs-moody
   emacs-olivetti
   emacs-ligature))

(define-public tree-sitter-packages
  (list tree-sitter-c
        tree-sitter-python
        tree-sitter-org
        tree-sitter-javascript
        tree-sitter-bash))

(define-public emacs-services
  (list
    (service home-emacs-service-type
             (home-emacs-configuration
              (package emacs-next-tree-sitter)
              ;; (rebuild-elisp-packages? #t)
              (elisp-packages emacs-packages)
              (init-el
               (list (slurp-file-like (local-file "../files/init.el"))))
              (early-init-el
               (list (slurp-file-like (local-file "../files/early-init.el"))))))))
