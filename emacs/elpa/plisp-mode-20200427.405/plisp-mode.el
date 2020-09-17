;;; plisp-mode.el --- Major mode for PicoLisp programming.

;; Copyright (C) 2014-2019  Alexis <flexibeast@gmail.com>

;; Author: Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; Created: 2014-11-18
;; URL: https://github.com/flexibeast/plisp-mode
;; Keywords: picolisp, lisp, programming

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; `plisp-mode' provides a major mode for PicoLisp programming.

;; The `plisp-mode' in this package has been built from scratch, and
;; is not based on, nor connected with, the PicoLisp support for Emacs
;; provided in [the PicoLisp
;; distribution](http://software-lab.de/down.html), or the more
;; recently [updated version of that
;; support](https://github.com/tj64/picolisp-mode). At this stage, the
;; main advantages provided by this package are:

;; * an actively maintained and supported system;

;; * access to the PicoLisp reference documentation, including via
;;   Eldoc;

;; * basic Imenu support;

;; * ease of customisability; and

;; * a cleaner codebase.

;; ## Table of Contents

;; - [Features](#features)
;; - [Installation](#installation)
;; - [Usage](#usage)
;;  - [Syntax highlighting](#highlighting)
;;  - [REPL](#repl)
;;  - [Inferior PicoLisp](#inferior-picolisp)
;;   - [Org Babel](#org-babel)
;;  - [Documentation](#documentation)
;;  - [Commenting](#commenting)
;;  - [Indentation](#indentation)
;;  - [Miscellaneous](#miscellanous)
;; - [TODO](#todo)
;; - [Issues](#issues)
;; - [License](#license)

;; ## Features

;; * Syntax highlighting of PicoLisp code. (But please read the below
;;   [note on syntax highlighting](#note-highlighting).)

;; * Comint-based `pil' REPL buffers.

;; * Quick access to documentation for symbol at point.

;; ## Installation

;; Install [plisp-mode from
;; MELPA](http://melpa.org/#/plisp-mode), or put the
;; `plisp-mode' folder in your load-path and do a `(require
;; 'plisp-mode)'.

;; ## Usage

;; <a name='highlighting'></a>

;; ### Syntax highlighting

;; Enable syntax highlighting for a PicoLisp source buffer with `M-x
;; plisp-mode'.

;; ### REPL

;; Start a `pil' REPL session with `M-x plisp-repl' or, from a
;; `plisp-mode' buffer, with `C-c C-i' (`plisp-repl').

;; <a name='inferior-picolisp'></a>

;; ### Inferior PicoLisp

;; This package provides the `inferior-plisp' feature, a fork of the
;; [`inferior-picolisp' library written by Guillermo Palavecino and
;; Thorsten Jolitz](https://github.com/tj64/picolisp-mode/), modified
;; to be compatible with `plisp-mode'.

;; By default, `inferior-plisp' is loaded by `plisp-mode'; to disable
;; this, set the variable `plisp-use-inferior-plisp' to `nil'. It can
;; still be manually loaded with `(require 'inferior-plisp)'.

;; With `inferior-plisp' loaded, the following bindings are available
;; in `plisp-mode' and `plisp-repl-mode':

;; * `M-C-x' / `C-c C-e' : Send the current definition to the inferior PicoLisp
;;   process (`inferior-plisp-send-definition').

;; * `C-x C-e' : Send the last sexp before point to the inferior
;;   PicoLisp process (`inferior-plisp-send-last-sexp').

;; * `C-c M-e' : Send the current definition to the inferior PicoLisp
;;   process and switch to its buffer
;;   (`inferior-plisp-send-definition-and-go').

;; * `C-c C-r' : Send the region to the inferior PicoLisp process
;;   (`inferior-plisp-send-region').

;; * `C-c M-r' : Send the region to the inferior PicoLisp process and
;;   switch to its buffer (`inferior-plisp-send-region-and-go').

;; * `C-c C-l' : Load a PicoLisp file into the inferior PicoLisp
;;   process (`inferior-plisp-load-file')."

;; * `C-c C-x' : Switch to the inferior PicoLisp buffer
;;   (`inferior-plisp-switch-to-picolisp').

;; Multiple inferior PicoLisp processes can be created and used; the
;; documentation for the variable `inferior-plisp-picolisp-buffer'
;; provides more details.

;; By default, `inferior-plisp' provides the feature
;; `inferior-picolisp' required by `ob-picolisp'. To use another
;; package to provide `inferior-picolisp', set the
;; `inferior-plisp-provide-inferior-picolisp' variable to `nil'.

;; <a name='org-babel'></a>

;; #### Org Babel

;; By default, `plisp-mode' registers itself as providing the
;; `picolisp-mode' needed to edit Org Babel PicoLisp source blocks
;; with `org-edit-special'. If you wish to disable this, set the
;; variable `plisp-provide-picolisp-mode' to `nil'.

;; `inferior-plisp' can support Org Babel sessions: add
;; `(inferior-plisp-support-ob-picolisp)' to your init file, and make
;; sure the `org-babel-picolisp-cmd' variable defined by `ob-picolisp'
;; is correctly specified for your system.

;; ### Documentation

;; Access documentation for the function at point with `C-c C-d'
;; (`plisp-describe-symbol'). By default, documentation will be
;; displayed via the `lynx' HTML browser. However, one can set the
;; value of `plisp-documentation-method' to either a string
;; containing the absolute path to an alternative browser, or - for
;; users of Emacs 24.4 and above - to the symbol
;; `plisp--shr-documentation'; this function uses the `shr' library
;; to display the documentation in an Emacs buffer. The absolute path
;; to the documentation is specified via
;; `plisp-documentation-directory', and defaults to
;; `/usr/share/doc/picolisp/'.

;; Eldoc support is available.

;; If for some reason the PicoLisp documentation is not installed on
;; the system, and cannot be installed, setting
;; `plisp-documentation-unavailable' to `t' will prevent
;; `plisp-mode' from trying to provide documentation.

;; ### Commenting

;; Comment a region in a `plisp-mode' buffer with `C-c C-;'
;; (`plisp-comment-region'); uncomment a region in a
;; `plisp-mode' buffer with `C-c C-:'
;; (`plisp-uncomment-region'). By default one '#' character is
;; added/removed; to specify more, supply a numeric prefix argument to
;; either command.

;; ### Indentation

;; Indent a region in a `plisp-mode' buffer with `C-c M-q'
;; (`plisp-indent-region'). Indentation is done via the `pilIndent'
;; script provided with the current PicoLisp distribution; the path to
;; the script is specified via the `plisp-pilindent-executable'
;; variable.

;; ### Miscellaneous

;; SLIME users should read the below [note on SLIME](#note-slime).

;; The various customisation options, including the faces used for
;; syntax highlighting, are available via the `plisp'
;; customize-group.

;; <a name="note-highlighting"></a>

;; ### A note on syntax highlighting

;; PicoLisp's creator is opposed to syntax highlighting of symbols in
;; PicoLisp, for [good
;; reasons](http://www.mail-archive.com/picolisp@software-lab.de/msg05019.html).
;; However, some - such as the author of this package! - feel that,
;; even taking such issues into consideration, the benefits can
;; outweigh the costs. (For example, when learning PicoLisp, it can be
;; useful to get immediate visual feedback about unintentionally
;; redefining a PicoLisp 'builtin'.) To accommodate both views, syntax
;; highlighting can be enabled or disabled via the
;; `plisp-syntax-highlighting-p' variable; by default, it is set to
;; `t' (enabled).

;; <a name="note-slime"></a>

;; ### A note on [SLIME](https://github.com/slime/slime)

;; The design of SLIME is such that it can override `plisp-mode'
;; functionality. (The documentation for
;; `plisp--disable-slime-modes' provides details.) The
;; user-customisable variable `plisp-disable-slime-p' specifies
;; whether to override these overrides, and defaults to `t'.

;; ## TODO

;; * Fix misalignment of single-'#' comments upon newline.

;; <a name="issues"></a>

;; ## Issues / bugs

;; If you discover an issue or bug in `plisp-mode' not already
;; noted:

;; * as a TODO item, or

;; * in [the project's "Issues" section on
;;   GitHub](https://github.com/flexibeast/plisp-mode/issues),

;; please create a new issue with as much detail as possible,
;; including:

;; * which version of Emacs you're running on which operating system,
;;   and

;; * how you installed `plisp-mode'.

;; ## License

;; [GNU General Public License version
;; 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any
;; later version.

;;; Code:


;;
;; User-customisable settings.
;;

(defgroup plisp nil
  "PicoLisp support."
  :group 'languages)

(defcustom plisp-disable-slime-p t
  "Whether to disable SLIME modes in `plisp-mode' buffers."
  :type 'boolean
  :group 'plisp)

(defcustom plisp-documentation-directory "/usr/share/doc/picolisp/"
  "Absolute path of the PicoLisp HTML documentation directory."
  :type 'directory
  :group 'plisp)

(defcustom plisp-documentation-method 'plisp--shr-documentation
  "System to be used to display PicoLisp documentation."
  :type '(radio (function :tag "Function - must already be defined"
                          :value 'plisp--shr-documentation)
                (file :tag "HTML browser - absolute path"
                      :value "/usr/bin/lynx"))
  :group 'plisp)

(defcustom plisp-documentation-unavailable nil
  "Whether the PicoLisp documentation is unavailable on the system.

If for some reason the PicoLisp documentation is not installed on
the system, and cannot be installed, setting this to `t' will prevent
`plisp-mode' from trying to provide documentation."
  :type 'boolean
  :group 'plisp)

(defcustom plisp-picolisp-executable "/usr/bin/picolisp"
  "Absolute path of the `picolisp' executable."
  :type '(file :must-match t)
  :group 'plisp)

(defcustom plisp-pil-executable "/usr/bin/pil"
  "Absolute path of the `pil' executable."
  :type '(file :must-match t)
  :group 'plisp)

(defcustom plisp-pilindent-executable "/usr/bin/pilIndent"
  "Absolute path of the `pilIndent' executable."
  :type '(file :must-match t)
  :group 'plisp)

(defcustom plisp-provide-picolisp-mode t
  "Compatibility option for `ob-picolisp'.

When set to `t', `plisp-mode' will register itself as providing
the `picolisp-mode' feature required by `ob-picolisp' to edit
PicoLisp source blocks via `org-edit-special'.

Set this to `nil' to if you wish to use another package to
provide the `picolisp-mode' feature to `ob-picolisp'."
  :type 'boolean
  :group 'plisp)

(defcustom plisp-repl-debug-p t
  "Whether to enable debug mode in the REPL.
Must be `t' to access documentation via `plisp-describe-symbol'."
  :type 'boolean
  :group 'plisp)

(defcustom plisp-syntax-highlighting-p t
  "Whether to enable syntax highlighting."
  :type 'boolean
  :group 'plisp)

(defcustom plisp-use-inferior-plisp t
  "Whether to enable `inferior-plisp' functionality."
  :type 'boolean
  :group 'plisp)

(defgroup plisp-faces nil
  "Faces for PicoLisp syntax highlighting."
  :group 'plisp)

(defface plisp-abstract-class-face
  '((t :inherit font-lock-type-face))
  "Face for PicoLisp abstract classes."
  :group 'plisp-faces)

(defface plisp-at-mark-face
  '((t :inherit font-lock-variable-name-face))
  "Face for PicoLisp at-marks."
  :group 'plisp-faces)

(defface plisp-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face for PicoLisp builtins."
  :group 'plisp-faces)

(defface plisp-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for PicoLisp comments."
  :group 'plisp-faces)

(defface plisp-global-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for PicoLisp global constants."
  :group 'plisp-faces)

(defface plisp-global-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face for PicoLisp global variables."
  :group 'plisp-faces)

(defface plisp-local-function-face
  '((t :inherit font-lock-function-name-face))
  "Face for PicoLisp local functions."
  :group 'plisp-faces)

(defface plisp-method-face
  '((t :inherit font-lock-function-name-face))
  "Face for PicoLisp methods."
  :group 'plisp-faces)

(defface plisp-normal-class-face
  '((t :inherit font-lock-type-face))
  "Face for PicoLisp normal classes."
  :group 'plisp-faces)

(defface plisp-transient-symbol-face
  '((t :inherit font-lock-string-face))
  "Face for PicoLisp transient symbols."
  :group 'plisp-faces)


;; Initialise.
;;
;;

(if plisp-use-inferior-plisp
    (require 'inferior-plisp))


;;
;; Internal variables.
;;

(defvar plisp-mode-map (make-sparse-keymap))
(define-key plisp-mode-map (kbd "C-c C-;") 'plisp-comment-region)
(define-key plisp-mode-map (kbd "C-c C-:") 'plisp-uncomment-region)
(define-key plisp-mode-map (kbd "C-c C-d") 'plisp-describe-symbol)
(define-key plisp-mode-map (kbd "C-c C-i") 'plisp-repl)
(define-key plisp-mode-map (kbd "C-c M-q") 'plisp-indent-region)
(if plisp-use-inferior-plisp
    (progn
      (define-key plisp-mode-map (kbd "M-C-x") 'inferior-plisp-send-definition)
      (define-key plisp-mode-map (kbd "C-x C-e") 'inferior-plisp-send-last-sexp)
      (define-key plisp-mode-map (kbd "C-c C-e") 'inferior-plisp-send-definition)
      (define-key plisp-mode-map (kbd "C-c M-e") 'inferior-plisp-send-definition-and-go)
      (define-key plisp-mode-map (kbd "C-c C-r") 'inferior-plisp-send-region)
      (define-key plisp-mode-map (kbd "C-c M-r") 'inferior-plisp-send-region-and-go)
      (define-key plisp-mode-map (kbd "C-c C-l") 'inferior-plisp-load-file)
      (define-key plisp-mode-map (kbd "C-c C-x") 'inferior-plisp-switch-to-picolisp)))

(defvar plisp-repl-mode-map (make-sparse-keymap))
(define-key plisp-repl-mode-map (kbd "C-c C-d") 'plisp-describe-symbol)
(if plisp-use-inferior-plisp
    (progn
      (define-key plisp-repl-mode-map (kbd "M-C-x") 'inferior-plisp-send-definition)
      (define-key plisp-repl-mode-map (kbd "C-x C-e") 'inferior-plisp-send-last-sexp)
      (define-key plisp-mode-map (kbd "C-c C-e") 'inferior-plisp-send-definition)
      (define-key plisp-mode-map (kbd "C-c M-e") 'inferior-plisp-send-definition-and-go)
      (define-key plisp-mode-map (kbd "C-c C-r") 'inferior-plisp-send-region)
      (define-key plisp-mode-map (kbd "C-c M-r") 'inferior-plisp-send-region-and-go)
      (define-key plisp-mode-map (kbd "C-c C-l") 'inferior-plisp-load-file)
      (define-key plisp-mode-map (kbd "C-c C-x") 'inferior-plisp-switch-to-picolisp)))


;; http://software-lab.de/doc/ref.html#fun

(defvar plisp-builtins
  '("!" "$" "$dat" "$tim" "%" "&" "*" "**" "*/" "*Allow" "*Bye" "*CPU" "*Class" "*Class" "*DB" "*Dbg" "*Dbg" "*Dbs" "*EAdr" "*Err" "*Fork" "*Hup" "*Led" "*Msg" "*OS" "*PPid" "*Pid" "*Prompt" "*Run" "*Scl" "*Sig1" "*Sig2" "*Solo" "*Tsm" "*Uni" "*Zap" "+" "+Alt" "+Any" "+Aux" "+Bag" "+Blob" "+Bool" "+Date" "+Dep" "+Entity" "+Fold" "+Hook" "+Hook2" "+Idx" "+IdxFold" "+Joint" "+Key" "+Link" "+List" "+Mis" "+Need" "+Number" "+Ref" "+Ref2" "+Sn" "+String" "+Swap" "+Symbol" "+Time" "+UB" "+index" "+relation" "-" "->" "/" ":" "::" ";" "<" "<=" "<>" "=" "=0" "=:" "==" "====" "=T" ">" ">=" ">>" "?" "@" "@@" "@@@" "This" "^" "abort" "abs" "accept" "accu" "acquire" "adr" "alarm" "align" "all" "allow" "allowed" "and" "any" "append" "append/3" "apply" "arg" "args" "argv" "as" "asoq" "assert" "asserta" "asserta/1" "assertz" "assertz/1" "assoc" "at" "atom" "aux" "balance" "be" "beep" "bench" "bin" "bind" "bit?" "blob" "blob!" "bool" "bool/3" "box" "box?" "by" "bye" "bytes" "caaaar" "caaadr" "caaar" "caadar" "caaddr" "caadr" "caar" "cache" "cadaar" "cadadr" "cadar" "caddar" "cadddr" "caddr" "cadr" "call" "call/1" "can" "car" "case" "casq" "catch" "cd" "cdaaar" "cdaadr" "cdaar" "cdadar" "cdaddr" "cdadr" "cdar" "cddaar" "cddadr" "cddar" "cdddar" "cddddr" "cdddr" "cddr" "cdr" "center" "chain" "char" "chdir" "chkTree" "chop" "circ" "circ?" "class" "clause" "clause/2" "clip" "close" "cmd" "cnt" "co" "collect" "commit" "con" "conc" "cond" "connect" "cons" "copy" "count" "ctl" "ctty" "curry" "cut" "d" "daemon" "dat$" "datStr" "datSym" "date" "day" "db" "db/3" "db/4" "db/5" "db:" "dbSync" "dbck" "dbs" "dbs+" "de" "debug" "dec" "def" "default" "del" "delete" "delete/3" "delq" "dep" "depth" "diff" "different/2" "dir" "dirname" "dm" "do" "doc" "e" "echo" "edit" "em" "env" "eof" "eol" "equal/2" "err" "errno" "eval" "expDat" "expTel" "expr" "ext?" "extend" "extern" "extra" "extract" "fail" "fail/0" "fetch" "fifo" "file" "fill" "filter" "fin" "finally" "find" "fish" "flg?" "flip" "flush" "fmt64" "fold" "fold/3" "for" "fork" "forked" "format" "free" "from" "full" "fully" "fun?" "gc" "ge0" "genKey" "get" "getd" "getl" "glue" "goal" "group" "gt0" "hash" "hax" "hd" "head" "head/3" "heap" "hear" "here" "hex" "host" "id" "idx" "if" "if2" "ifn" "import" "in" "inc" "inc!" "index" "info" "init" "insert" "intern" "ipid" "isa" "isa/2" "iter" "job" "journal" "key" "kids" "kill" "last" "later" "ld" "le0" "leaf" "length" "let" "let?" "lieu" "line" "lines" "link" "lint" "lintAll" "list" "listen" "lit" "load" "loc" "local" "locale" "lock" "loop" "low?" "lowc" "lst/3" "lst?" "lt0" "lup" "macro" "made" "mail" "make" "map" "map/3" "mapc" "mapcan" "mapcar" "mapcon" "maplist" "maps" "mark" "match" "max" "maxKey" "maxi" "member" "member/2" "memq" "meta" "meth" "method" "min" "minKey" "mini" "mix" "mmeq" "money" "more" "msg" "n0" "n==" "nT" "name" "nand" "native" "need" "new" "new!" "next" "nil" "nil/1" "nond" "nor" "not" "not/1" "nth" "num?" "obj" "object" "oct" "off" "offset" "on" "onOff" "once" "one" "open" "opid" "opt" "or" "or/2" "out" "pack" "pad" "pair" "part/3" "pass" "pat?" "patch" "path" "peek" "permute/2" "pick" "pico" "pilog" "pipe" "place" "poll" "pool" "pop" "port" "pp" "pr" "prEval" "pre?" "pretty" "prin" "prinl" "print" "println" "printsp" "prior" "proc" "prog" "prog1" "prog2" "prop" "protect" "prove" "prune" "push" "push1" "put" "put!" "putl" "pwd" "qsym" "query" "queue" "quit" "quote" "rand" "range" "range/3" "rank" "raw" "rc" "rd" "read" "recur" "recurse" "redef" "rel" "release" "remote/2" "remove" "repeat" "repeat/0" "replace" "request" "rest" "retract" "retract/1" "reverse" "rewind" "rollback" "root" "rot" "round" "rules" "run" "same/3" "scan" "scl" "script" "sect" "seed" "seek" "select" "select/3" "send" "seq" "set" "set!" "setq" "show" "show/1" "sigio" "size" "skip" "solve" "sort" "sp?" "space" "split" "sqrt" "stack" "stamp" "state" "stem" "step" "store" "str" "str?" "strDat" "strip" "sub?" "subr" "sum" "super" "sym" "sym?" "symbols" "sync" "sys" "t" "tab" "tail" "task" "telStr" "tell" "test" "text" "throw" "tick" "till" "tim$" "time" "timeout" "tmp" "tolr/3" "touch" "trace" "traceAll" "trail" "tree" "trim" "true/0" "try" "type" "u" "ubIter" "udp" "ultimo" "unbug" "undef" "unify" "uniq" "uniq/2" "unless" "until" "untrace" "up" "upd" "update" "upp?" "uppc" "use" "useKey" "usec" "val" "val/3" "var" "var:" "version" "vi" "view" "wait" "week" "what" "when" "while" "who" "wipe" "with" "wr" "wrap" "xchg" "xor" "x|" "yield" "yoke" "zap" "zapTree" "zero" "|"))

(defvar plisp-builtins-by-length
  (let ((bs (copy-sequence plisp-builtins)))
    (sort bs #'(lambda (e1 e2)
                 (> (length e1) (length e2)))))
  "List of PicoLisp builtins, sorted by length for use by
`plisp-builtins-regex'.")

(defvar plisp-builtins-regex
  (let ((s "")
        (firstp t))
    (dolist (b plisp-builtins-by-length)
      (if (not firstp)
          (setq s (concat s "\\|" (regexp-quote b)))
        (progn
          (setq s (regexp-quote b))
          (setq firstp nil))))
    s)
  "Regex for use by `plisp-font-lock-keywords'.")

;; http://software-lab.de/doc/ref.html#conv

(defvar plisp-font-lock-keywords
  `(("\\_<\\(T\\|NIL\\)\\_>"
     (1 'plisp-global-constant-face t))
    ("\\(\\*[^]\\\"'(),[`~{}[:space:]]+\\)"
     (1 'plisp-global-variable-face t))
    ("\\(\\+[a-z]\\S-*\\)"
     (1 'plisp-abstract-class-face t))
    ("\\(\\+[A-Z][[:alpha:]]*\\)"
     (1 'plisp-normal-class-face t))
    (,(concat "\\((\\|\\[\\)\\_<\\(" plisp-builtins-regex "\\)\\_>")
     (1 'default t)
     (2 'plisp-builtin-face t))
    ("(\\(_\\S-+\\)"
     (1 'plisp-local-function-face t))
    ("(\\([[:alpha:]]\\S-+>\\s-\\)"
     (1 'plisp-method-face t))
    ("\\(\".+?\"\\)"
     (1 'plisp-transient-symbol-face t))
    ("\\(@[0-9A-Z]?\\)"
     (1 'plisp-at-mark-face t))
    ("^.*?\\(#+.*\\)$"
     (1 'plisp-comment-face t)))
  "Regexes for syntax-highlighting `plisp-mode' buffers.")

;;
;; http://software-lab.de/doc/ref.html#symbol:
;;
;; Internal symbol names can consist of any printable (non-whitespace)
;; character, except for the following meta characters:
;;
;; "  '  (  )  ,  [  ]  `  ~ { }
;;

(defvar plisp-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))

    ;; " primarily indicates a transient symbol, even
    ;; though it can also be used to indicate strings.
    (modify-syntax-entry ?\" "_   " table)

    ;; Comment syntax.
    (modify-syntax-entry ?\# "<   " table)

    ;; '[' and ']' can delimit sexps.
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)

    table)

  "Syntax table used in `plisp-mode'.")


;;
;; Internal functions.
;;

(defun plisp--create-plisp-mode-menu ()
  "Internal function to create or recreate the plisp-mode menu."
  (easy-menu-define plisp-menu plisp-mode-map "Menu bar entry for `plisp-mode'"
    `("PicoLisp"
      ["Comment region" (plisp-comment-region) :keys "C-c C-;"]
      ["Uncomment region" (plisp-uncomment-region) :keys "C-c C-:"]
      ["Indent region" (plisp-indent-region) :keys "C-c M-q"]
      ,@(if plisp-use-inferior-plisp
            (list
             ["Send last sexp" (inferior-plisp-send-last-sexp) :keys "C-x C-e"]
             ["Send definition" (inferior-plisp-send-definition) :keys "M-C-x"]
             ["Send definition and go" (inferior-plisp-send-definition-and-go) :keys "C-c M-e"]
             ["Switch to *picolisp* buffer" (inferior-plisp-switch-to-picolisp) :keys "C-c C-x"]))
      ["PicoLisp REPL" (plisp-repl) :keys "C-c C-i"]
      ["Customize" (customize-group 'plisp) t])))

(defun plisp--disable-slime-modes ()
  "Function to add to `lisp-mode-hook' if `plisp-disable-slime-p'
is set to `t'.

SLIME adds the function `slime-lisp-mode-hook' to the
`lisp-mode-hook' variable. Since `plisp-mode' is defined as
being derived from `lisp-mode', the effect of this is to enable
various SLIME features in `plisp-mode' buffers, overriding
`plisp-mode' functionality.

This function thus overrides those overrides, and:

* disables `slime-mode';

* disables `slime-autodoc-mode'; and

* ensures that the value of `eldoc-documentation-function' is
  `plisp--eldoc-function'."
  (and (fboundp 'slime-mode) (slime-mode 0))
  (and (fboundp 'slime-autodoc-mode) (slime-autodoc-mode 0))
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function #'plisp--eldoc-function))

(defun plisp--eldoc-function ()
  "Function for use by `eldoc-documentation-function'."
  (unless plisp-documentation-unavailable
    (let* ((sym (symbol-name (symbol-at-point)))
           (dl (plisp--extract-reference-documentation sym))
           (result nil))
      (if (string-or-null-p dl)
          (if (y-or-n-p "Documentation not found. (Check the value of `plisp-documentation-directory', or set `plisp-documentation-unavailable' to `t'.) Turn off Eldoc mode in this buffer? ")
              (eldoc-mode 0))
        (unless (string= "nil" sym)
          (dotimes (i (/ (length dl) 2))
            (let ((fst (nth (* i 2) dl))
                  (snd (nth (1+ (* i 2)) dl)))
              (if (eq 'dt (car-safe fst))
                  (cond
                   ((eq 'cons (type-of (nth 2 fst)))
                    (if (string= sym (cdaadr (nth 2 fst)))
                        (setq result (concat (propertize sym 'face 'plisp-builtin-face)
                                             ", "
                                             (nth 2 (caddr (nth 2 fst)))))))
                   ;; Handle the documentation for `c[ad]*[ad]r'.
                   ((eq 'string (type-of (nth 2 fst)))
                    (if (string= "cXr" (cdaadr (nth 59 fst)))
                        (setq result (concat (propertize "c[ad]*ar" 'face 'plisp-builtin-face)
                                             ", "
                                             "(c[ad]*ar 'var) -> any"
                                             "; "
                                             (propertize "c[ad]*dr" 'face 'plisp-builtin-face)
                                             ", "
                                             "(c[ad]*dr 'lst) -> any"))
                      ;; Ignore any other edge-cases in the documentation structure.
                      (setq result nil)))))))))
      result)))

(defun plisp--extract-reference-documentation (sym)
  "Helper function to extract the 'Function Reference' definition
list from the PicoLisp documentation, where SYM is the symbol being
looked up."
  (let* ((dl "Documentation not found. Please check the value of `plisp-documentation-directory'")
         (char (progn
                 (string-match "^[[:punct:]]*\\([[:punct:]]\\|[[:alpha:]]\\)" sym)
                 (upcase (match-string 1 sym))))
         (doc (if (string-match "[[:alpha:]]" char)
                  (concat plisp-documentation-directory "/ref" char ".html")
                (concat plisp-documentation-directory "/ref_.html"))))
    (if (file-readable-p doc)
        (let* ((bfr (generate-new-buffer " *PicoLisp documentation source*"))
               (dom (progn
                      (switch-to-buffer bfr)
                      (insert-file-contents doc)
                      (libxml-parse-html-region (point-min) (point-max)))))
          (setq dl (nth 5 (nth 3 dom)))
          (kill-buffer bfr)))
    dl))

(defun plisp--font-lock-syntactic-face-function (state)
  "No-op function to prevent font-lock from trying to fontify
comments and strings.

Since strings in PicoLisp are fundamentally (transient)
symbols, they are marked as such in the PicoLisp syntax-table.
However, this makes it complicated for Emacs to determine if
a '#' character is a comment delimiter or merely a constituent
of a string / transient symbol. So we override syntactic
fontification with this no-op function, and fontify comments
via `plisp-font-lock-keywords'."
  nil)

(defun plisp--imenu-create-index ()
  "Internal function to create an Imenu index."
  (let ((index '()))
    (setq index (append index (plisp--imenu-find-classes-and-members)))
    (setq index (append index (plisp--imenu-find-database-objects)))
    (setq index (append index (plisp--imenu-find-facts-and-rules)))
    (setq index (append index (plisp--imenu-find-functions)))
    (setq index (append index (plisp--imenu-find-global-variables)))
    index))

(defun plisp--imenu-find-classes-and-members ()
  "Internal function to find PicoLisp classes and their
associated methods and/or relations."
  (let ((classes '()))
    (goto-char (point-min))
    (while (re-search-forward
            "^[[:space:]]*(class \\([+][[:alnum:]]+\\)" nil t)
      (let ((class (match-string 1))
            (class-index (match-beginning 1))
            (members '())
            (methods '())
            (relations '())
            (next-class-index 0))
        (setq members `(("Definition" . ,class-index)))
        (save-excursion
          (setq next-class-index
                (if (re-search-forward
                     "^[[:space:]]*(class \\([+][[:alnum:]]+\\)" nil t)
                    (match-beginning 1)
                  (point-max))))
        (save-excursion
          (while (re-search-forward "^[[:space:]]*(dm \\([[:alnum:]]+>\\)"
                                    next-class-index
                                    t)
            (setq methods
                  (append methods
                          `((,(match-string 1) . ,(match-beginning 1)))))))
        (setq methods `(("Methods" . ,methods)))
        (save-excursion
          (while (re-search-forward
                  "^[[:space:]]*(rel \\([[:alnum:]]+\\)" next-class-index t)
            (setq relations
                  (append relations
                          `((,(match-string 1) . ,(match-beginning 1)))))))
        (setq relations `(("Relations" . ,relations)))
        (setq members (append members methods relations))
        (setq classes (append classes `((,class . ,members))))))
    (setq classes `(("Classes" . ,classes)))
    classes))

(defun plisp--imenu-find-database-objects ()
  "Internal function to find PicoLisp database objects."
  (let ((re (concat "^[[:space:]]*(obj[[:space:]]+((\\([^)]+\\))[[:space:]]+"
                    "\\(?:[^[:space:]]+[[:space:]]+\\)?\\([^[:space:]]+\\))"))
        (objs '()))
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (let ((obj-class (match-string 1))
            (obj-identifier (match-string 2))
            (obj-position (match-beginning 2)))
        (if (assoc obj-class objs)
            (setcdr (assoc obj-class objs)
                    (append (cdr (assoc obj-class objs))
                            `((,obj-identifier . ,obj-position))))
          (setq objs
                (append objs
                        `((,obj-class . ((,obj-identifier . ,obj-position)))))))))
    (setq objs `(("Database objects" . ,objs)))
    objs))

(defun plisp--imenu-find-facts-and-rules ()
  "Internal function to find PicoLisp facts and/or rules."
  (plisp--imenu-find-things
   "Facts and rules"
   "^[[:space:]]*(be \\([[:alnum:]]+\\))"))

(defun plisp--imenu-find-functions ()
  "Internal function to find PicoLisp functions."
  (plisp--imenu-find-things
   "Functions"
   "^[[:space:]]*(de \\([^*][[:alnum:]*+_]+\\)[[:space:]]+("))

(defun plisp--imenu-find-global-variables ()
  "Internal function to find PicoLisp global variables."
  (plisp--imenu-find-things
   "Global variables"
   "^[[:space:]]*(de \\([*][[:alnum:]*+]+\\)[[:space:]]+"))

(defun plisp--imenu-find-things (name re)
  "Internal function to find PicoLisp components of type NAME
that can be identified by a simple regular expression RE."
  (let ((things '()))
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (setq things (append things
                           `((,(match-string 1) . ,(match-beginning 1))))))
    (setq things `((,name . ,things)))
    things))

(defun plisp--shr-documentation (sym)
  "Use `shr' to display documentation for symbol SYM at point."
  (unless plisp-documentation-unavailable
    (unless (or (> emacs-major-version 24)
                (and (= emacs-major-version 24)
                     (> emacs-minor-version 3)))
      (error "Emacs 24.4 or greater required"))
    (let ((dl (plisp--extract-reference-documentation sym)))
      (if (string-or-null-p dl)
          (user-error dl))
      (dotimes (i (/ (length dl) 2))
        (let ((fst (nth (* i 2) dl))
              (snd (nth (1+ (* i 2)) dl)))
          (if (eq 'dt (car-safe fst))
              (cond
               ((eq 'cons (type-of (nth 2 fst)))
                (if (string= sym (cdaadr (nth 2 fst)))
                    (progn
                      (switch-to-buffer
                       (generate-new-buffer
                        (concat "*PicoLisp documentation - '" sym "' *")))
                      (insert
                       (concat
                        (propertize "Symbol:"
                                    'face '(foreground-color . "ForestGreen"))
                        " "
                        (propertize sym
                                    'face 'plisp-builtin-face)
                        "\n\n"))
                      (shr-insert-document snd)
                      (goto-char (point-min))
                      (help-mode))))
               ((eq 'string (type-of (nth 2 fst)))
                ;; Handle the documentation for `c[ad]*[ad]r'.
                (if (string= "cXr" (cdaadr (nth 59 fst)))
                    (progn
                      (switch-to-buffer
                       (generate-new-buffer
                        (concat "*PicoLisp documentation - 'cXr' *")))
                      (insert
                       (concat
                        (propertize "Symbol:"
                                    'face '(foreground-color . "ForestGreen"))
                        " "
                        (propertize "c[ad]*[ad]r"
                                    'face 'plisp-builtin-face) "\n\n"))
                      (shr-insert-document snd)
                      (goto-char (point-min))
                      (help-mode))
                  ;; Ignore any other edge-cases in the documentation structure.
                  nil)))))))))


;;
;; User-facing functions.
;;

(defun plisp-comment-region (&optional n)
  "Comment lines in region using N '#' characters. N can be
specified by providing a numeric prefix argument; otherwise,
N defaults to 1."
  (interactive "p")
  (if n
      (comment-region (region-beginning) (region-end) n)
    (comment-region (region-beginning) (region-end) 1)))

(defun plisp-uncomment-region (&optional n)
  "Uncomment lines in region by removing N '#' characters. N can
be specified by providing a numeric prefix argument; otherwise,
N defaults to 1."
  (interactive "p")
  (if n
      (uncomment-region (region-beginning) (region-end) n)
    (comment-region (region-beginning) (region-end) 1)))

(defun plisp-indent-region ()
  "Indent the active region using the `pilIndent' script."
  (interactive)
  (unless (region-active-p)
    (user-error "Region is not active"))
  (let* ((beginning (region-beginning))
         (end (region-end)))
    (shell-command-on-region
     beginning end
     plisp-pilindent-executable
     nil t)))

(defun plisp-describe-symbol ()
  "Display documentation for symbol at point, via method
specified by `plisp-documentation-method'."
  (interactive)
  (unless plisp-documentation-unavailable
    (let ((process-environment
           (if (eq 'string (type-of plisp-documentation-method))
               (add-to-list 'process-environment
                            (concat "BROWSER=" plisp-documentation-method))
             process-environment))
          (sym (symbol-name
                (symbol-at-point))))
      (if (member sym plisp-builtins)
          (cond
           ((eq 'symbol (type-of plisp-documentation-method))
            (plisp--shr-documentation sym))
           ((eq 'string (type-of plisp-documentation-method))
            (start-process-shell-command
             "picolisp-doc" nil
             (concat "pil -\"doc (car (nth (argv) 3)\" -bye - '" sym "' +")))
           (t
            (error "Unexpected value type in plisp-documentation-method")))
        (message "No PicoLisp builtin at point.")))))

;;;###autoload
(define-derived-mode plisp-mode lisp-mode "PicoLisp"
  "Major mode for PicoLisp programming. Derived from lisp-mode.

\\{plisp-mode-map}"
  :group 'plisp
  :syntax-table plisp-mode-syntax-table

  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (if plisp-syntax-highlighting-p
      (setq-local font-lock-defaults
                  '(plisp-font-lock-keywords
                    nil nil nil nil
                    (font-lock-syntactic-face-function
                     . plisp--font-lock-syntactic-face-function))))
  (setq-local eldoc-documentation-function #'plisp--eldoc-function)
  (plisp--create-plisp-mode-menu)
  (setq-local imenu-create-index-function 'plisp--imenu-create-index)
  (setq-local imenu-sort-function 'imenu--sort-by-name)
  (imenu-add-menubar-index)
  (if plisp-disable-slime-p
      (progn
        (make-local-variable 'lisp-mode-hook)
        (add-hook 'lisp-mode-hook 'plisp--disable-slime-modes))))

;;;###autoload
(define-derived-mode plisp-repl-mode comint-mode "PicoLisp REPL"
  "Major mode for `pil' REPL sessions. Derived from comint-mode.

\\{plisp-repl-mode-map}"
  :group 'plisp
  :syntax-table plisp-mode-syntax-table

  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (if plisp-syntax-highlighting-p
      (setq-local font-lock-defaults
                  '(plisp-font-lock-keywords
                    nil nil nil nil
                    (font-lock-syntactic-face-function
                     . plisp--font-lock-syntactic-face-function))))
  (setq-local eldoc-documentation-function #'plisp--eldoc-function))

;;;###autoload
(defun plisp-repl ()
  "Start a `pil' session in a new `plisp-repl-mode' buffer."
  (interactive)
  (let ((process-environment
         (if (eq 'string (type-of plisp-documentation-method))
             (add-to-list 'process-environment
                          (concat "BROWSER=" plisp-documentation-method))
           process-environment)))
    (make-comint "picolisp-repl"
                 plisp-pil-executable nil (if plisp-repl-debug-p "+" nil))
    (switch-to-buffer "*picolisp-repl*")
    (plisp-repl-mode)))

;;;###autoload
(defun plisp-support-ob-picolisp ()
  "Enable editing of Org Babel PicoLisp source blocks with `plisp-mode'.

Needs `plisp-provide-picolisp-mode' set to `t'."
  (interactive)
  (if plisp-provide-picolisp-mode
      (progn
        (provide 'picolisp-mode) 
        (defalias 'picolisp-mode 'plisp-mode))
    (t (error "Unable to support ob-picolisp: please ensure 'plisp-provide-picolisp-mode' is set to 't'"))))


;; --

(plisp-support-ob-picolisp)
(provide 'plisp-mode)

;;; plisp-mode.el ends here
