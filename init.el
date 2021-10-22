;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a number of
;; other files.

;;; Code:

;; System type.
;; Currently, only Windows and Linux is supported, as I don't use other OSs.

(setq *win64* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))

;; Define config file locations.

(defconst zy/emacs-d
  (file-name-as-directory (file-truename user-emacs-directory))
  "The path of .emacs.d.")
(defconst zy/lisp-path (concat zy/emacs-d "lisp/")
  "The path of all my config files.")
(defconst zy/site-lisp-path (concat zy/emacs-d "site-lisp/")
  "The path of all external lisp files.")
(defconst zy/3rd-party-path (concat zy/emacs-d "3rd-party/")
  "The path of all 3rd-party tools.")

;; Fast loader for separate config files.

(defun zy/load (pkg &optional maybe-disabled at-root)
  "Load PKG if MAYBE-DISABLED is nil.

If AT-ROOT is non-nil, load the file at the .emacs.d directory."
  (unless maybe-disabled
    (load (concat
	   (file-name-as-directory
	    (if at-root zy/emacs-d zy/lisp-path))
	   (format "%s" pkg))
	  'noerror 'nomessage)))

;; Load benchmark utils.

(zy/load 'init-benchmark)

;; Load custom file if there is one.

(setq custom-file (concat zy/emacs-d "custom.el"))
(zy/load custom-file nil 'at-root)

;; Top level utilities and definitions.

(zy/load 'init-overall)
(zy/load 'init-mngt)
(zy/load 'init-encoding)

;; Visual things like themes and fonts.

(zy/load 'init-visual)

;; Feature sets.

(zy/load 'init-editing)
(zy/load 'init-files)
(zy/load 'init-lingua)
(zy/load 'init-outline)
(zy/load 'init-ui)
(zy/load 'init-vc)

;; Personal features.

(zy/load 'init-quick-access)

;; Config for different file types.

(zy/load 'init-elisp)
(zy/load 'init-latex)
(zy/load 'init-markdown)
(zy/load 'init-org)
(zy/load 'init-python)

(zy/load 'init-misc)

;; End of config.

(provide 'init)
