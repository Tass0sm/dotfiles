;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (home environment)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu home-services)
  ;; personal modules
  #:use-module (home modules xdg)
  #:use-module (home modules shell)
  #:use-module (home modules emacs)
  #:use-module (home modules git)
  #:use-module (home modules gtk)
  #:use-module (home modules desktop))

(define base-packages
  (map specification->package
       (list
	"alacritty"
	"hunspell"
	"hunspell-dict-en"
	"glibc-locales"
	"nss-certs")))

(define xfce-packages
  (map specification->package
       (list
	"xfce"
	"xfce4-session"
	"xfconf"
	"xfce4-battery-plugin"
	"xfce4-volumed-pulse"
	"xfce4-notifyd"
	"pulseaudio"
	"xbacklight"
	"pavucontrol")))

(home-environment
  (packages
   `(,@base-packages
     ,@xfce-packages))
  (services
   `(,@xdg-services
     ,@zsh-services
     ,@git-services
     ,@gtk-services
     ,@emacs-services
     ,@desktop-services)))
