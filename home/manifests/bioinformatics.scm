(define-module (home manifests bioinformatics)
  #:use-module (gnu packages))

(specifications->manifest
 '("python"
   "python-bsddb3"
   "python-numpy"
   "python-pytorch"
   "python-matplotlib"
   "zlib"
   "minimap2"))
