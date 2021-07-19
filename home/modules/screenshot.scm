(define-module (home modules screenshot)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services files))

(define-configuration home-flameshot-configuration
  (server-mode?
   (boolean #f)
   "Create a shepherd service, which starts emacs in a server-mode.  Use
can use @command{emacsclient} to connect to the server (@pxref{Emacs
Server,,,emacs.info}).")
  (init-el
   (elisp-config '())
   "List of expressions, each expression can be a Sexp or Gexp.
Sexp is a Emacs Lisp form, preferably valid.  Be aware, if you include
values of Guile variables, they won't be automatically converted to
Elisp.  Strings doesn't require conversion, but for example booleans
do: @code{#t} -> @code{t}, @code{#f} -> @code{nil}.  Be careful here.
However, Sexp can contain file-like objects; String with path to a
corresponding file will appear in place of each such object.  See an
example below for more details.
Gexp should be string-valued.  The value of Gexp will be appended to
resulting Emacs Lisp file.
The list of expressions will be interposed with \\n and everything
will end up in @file{init.el}.
@example
(let ((guile-bool-value #f))
  (home-emacs-configuration
   (init-el
    `((setq rg-binary ,(file-append ripgrep \"/bin/rg\"))
      (load-file ,(local-file \"./emacs/test-init.el\"))
      \"just a string\"
      ;; Make sure you converted guile values to Elisp
      (setq tmp-boolean ,(if guile-bool-value 't 'nil))
      ,(if guile-bool-value '(setq v1 nil) '(setq v2 t))
      ,#~\"\\n;;; Section with gexps results:\"
      ,(slurp-file-gexp (local-file \"./emacs/test-init.el\"))
      ,#~(string-append \"(princ \" \"'hello)\")
      ,#~\"\\n\"
      ,#~\";; Another comment\"))))
@end example
would yield something like:
@example
(setq rg-binary
      \"/gnu/store/dw884p9d2jb83j4fqvdj2i10fn9xgwqd-ripgrep-12.1.1/bin/rg\")
(load-file
  \"/gnu/store/9b1s48crng5dy9xmxskcdnillw18bkg2-test-init.el\")
\"just a string\"
(setq tmp-boolean nil)
(setq v2 t)
;;; Section with gexps results:
;; Here is
\"a sample\"
;; content of test-init.el
(princ 'hello)
;; Another comment
@end example")
  (early-init-el
   (elisp-config '())
   "List of expressions, each expression can be a Sexp or Gexp.
Same as @code{init-el}, but result will go to @file{early-init.el}."))

(define-public screenshot-services
  (list
   
   
   
   (simple-service 'gtk-config
 		   home-files-service-type
 		   `(("config/gtk-3.0/settings.ini"
                      ,(local-file "../files/settings.ini"))))))
