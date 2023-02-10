(define-module (home manifests cybersecurity)
  #:use-module (gnu packages))

(specifications->manifest
 '("netcat"
   "nmap"
   "radare2"
   "wireshark"))
