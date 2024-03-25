;;; init.el --- Boostrap config modules.  -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Check Emacs version.
(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Emacs %s or higher is required to run Zyxir's config" minver)))

;; Make these directories available to Emacs. They contain most files of this
;; configuration. Although all Lisp libraries and modules are loaded with their
;; full path in this file, populating `load-path' ensures correct dependency
;; management in byte compilation.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Enable/disable modules here.
(defvar zy-modules
  '(;; Use saner default values.
    +defaults
    ;; Platform or environment-specific.
    +terminal
    ;; Setup Evil, leader-prefixed keys, and key hints.
    +evil
    +leader
    +keyhint
    ;; Enhance the workbench.
    +theme
    +font
    +modeline
    +dashboard
    +linum
    +orderless
    +minibuffer
    +persist
    ;; Applications.
    +git
    +esup
    ;; Text-editing and coding.
    +corfu
    +dabbrev
    +paragraph
    +outline
    +quickins
    +search
    +yasnippet
    +syncheck
    +eldoc
    +undo
    +pair
    +editorconfig
    +direnv
    +eglot
    +treesit
    ;; Setup GCMH last to prevent GC during startup.
    +gcmh
    )
  "Enabled modules of Zyxir's Emacs configuration.")

;; Synchronize the configuration (re-compile everything, and install missing
;; packages in the process) if Emacs is started with the "--sync".
(when (member "--sync" command-line-args)
  ;; Delete the argument so that Dashboard setups its hooks normally.
  (setq command-line-args (delete "--sync" command-line-args))
  (message "Synchronizing the configuration...")
  (load (expand-file-name "zy-sync.el" user-emacs-directory)
        nil 'nomessage 'nosuffix)
  (message "Synchronizing the configuration...done"))

(defun zy-load-rel (relpath &rest args)
  "Load the file in relative path RELPATH.
RELPATH is relative to `user-emacs-directory', and is formatted
with ARGS like `format' does.

This function is and should only be used in the initialization
file (init.el)."
  (load (expand-file-name (apply #'format relpath args)
                          user-emacs-directory)
        ;; Announce the loading while debugging.
        nil (not init-file-debug) nil 'must-suffix))

(let (;; `file-name-handler-alist' is consulted each time a file is loaded.
      ;; Unsetting it speeds up startup notably.
      (file-name-handler-alist nil)
      ;; Each suffix of `load-suffixes' is tried while loading a file.
      ;; Temporarily dropping ".so" from the list reduces the permutations
      ;; needed to load the correct files during startup.
      (load-suffixes '(".elc" ".el"))
      ;; `load-source-file-function' decides what function to be called to do
      ;; code conversion before reading a source file. Since I do not need code
      ;; conversion while loading the configuration, unsetting it reduces tons
      ;; of startup time.
      (load-source-file-function nil)
      ;; Don't spend precious time checking modified time during startup. TODO:
      ;; mention a way to sync the configuration.
      (load-prefer-newer nil))

  ;; Load all components of Zylib manually, which defines utility functions and
  ;; macros that most parts of this configuration depend on. Load them by path
  ;; manually reduces the tiny bit of time used to locate them in `load-path'.
  (zy-load-rel "lisp/zylib-core")
  (zy-load-rel "lisp/zylib-pkg")
  (zy-load-rel "lisp/zylib-key")
  (zy-load-rel "lisp/zylib")

  ;; Load the custom file now.
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (zy-load-rel "custom.el")

  ;; Load all modules in order.
  (dolist (module zy-modules)
    (zy-load-rel "modules/zy-%s"
                 (substring (symbol-name module) 1))))

(provide 'init)

;;; init.el ends here
