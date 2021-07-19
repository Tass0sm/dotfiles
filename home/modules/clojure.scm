(define-module (home modules clojure)
  #:use-module (gnu packages))

(define-public clojure-packages
  (map specification->package
       (list
	"clojure"
	"leiningen"
	"emacs-cider")))
