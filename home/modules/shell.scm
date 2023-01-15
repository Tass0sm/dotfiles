(define-module (home modules shell)
  #:use-module (guix gexp)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module ((gnu home services shells)
                #:select (home-zsh-service-type
                          home-zsh-configuration))
  #:use-module (gnu home-services shellutils)
  ;; custom services and packages
  #:use-module (tassos-guix packages shellutils)
  #:use-module ((tassos-guix home-services shells) #:prefix my:))

(define-public zsh-packages
  (list direnv
	cache-env
        zsh-pure
        zsh-autosuggestions))

(define-public zsh-services
  (list
   (service my:home-zsh-service-type
            (my:home-zsh-configuration
             (xdg-flavor? #t)
             ;; Sourced in every shell (zshenv)
             (environment-variables
              '(("MONITOR" . "eDP")
                ("EDITOR" . "emacsclient -a ''")
                ("XCURSOR_THEME" . "Nordzy-cursors")
                ("SSH_AUTH_SOCK" . "/run/user/$(id -u)/gcr/ssh")
                ("SSL_CERT_DIR" . "$HOME/.guix-home/profile/etc/ssl/certs")
                ("SSL_CERT_FILE" . "$HOME/.guix-home/profile/etc/ssl/certs/ca-certificates.crt")
                ("GIT_SSL_CAINFO" . "$SSL_CERT_FILE")
                ("GEM_PATH" . "$HOME/.local/share/gem")
                ("_JAVA_AWT_WM_NONREPARENTING" . "1")
                ("DIRSTACKSIZE" . "8")))
             (zprofile
              (list
               (local-file "../files/zprofile")))
             (zshrc
              (list
               (local-file "../files/zshrc")))))
   (simple-service 'main-profile
                   my:home-shell-profile-service-type
                   (list (local-file "../files/profile")))
   (simple-service 'direnvrc
                   home-files-service-type
                   `((".config/direnv/direnvrc"
                      ,(local-file "../files/direnvrc"))))
   ;; Things sourced in the profile. Sourced in login shells when they're
   ;; configured to load .profile.
   (simple-service 'login-variables
                   home-environment-variables-service-type
                   `(("PATH" . "$HOME/.local/bin:$PATH")
                     ("GSETTINGS_SCHEMA_DIR" . "/usr/share/glib-2.0/schemas/")
                     ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:/usr/local/share:/usr/share")
                     ("XDG_CONFIG_DIRS" . "$XDG_CONFIG_DIRS:/etc/xdg")
		     ;; This should be loaded before any guile code is
		     ;; run, like the on-first-login script.
		     ("GUIX_LOCPATH" . "$HOME/.guix-home/profile/lib/locale")
		     ("GUIX_EXTRA_PROFILES" . "$HOME/.guix-extra-profiles")))))
