(define-module (home modules programming)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix packages))

(define-public programming-tool-packages
  (map specification->package
       (list
        "python-lsp-server")))
