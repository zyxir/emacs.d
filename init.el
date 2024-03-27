;;; init.el --- Boostrap config modules.  -*- lexical-binding: t -*-
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
    ;; Setup Evil, leader-prefixed keys, and key hints.
    +evil
    +leader
    +keyhint
    ;; Enhance the workbench.
    +theme
    +font
    +modeline
    +orderless
    +minibuffer
    +persist
    +dashboard
    +linum
    ;; Platform or environment-specific.
    +terminal
    +platform
    +personal
    ;; File/directory/project management.
    +project
    +direnv
    +file
    +dired
    ;; Applications or features.
    +git
    +gtd
    +journal
    +lingual
    ;; Text-editing and coding.
    +embark
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
    +eglot
    +treesit
    +prose
    ;; Programming Languages or file formats.
    +elisp
    +nix
    +org
    +pdf
    +python
    +scala
    +tex
    +othermodes
    ;; Setup GCMH last to prevent GC during startup.
    +gcmh)
  "Enabled modules of Zyxir's Emacs configuration.")

;; Synchronize the configuration (re-compile everything, and install missing
;; packages in the process) if Emacs is started with the "--sync".
(when-let* ((rest (or (member "--sync" command-line-args)
                      (member "--force-sync" command-line-args)))
            (switch (car rest)))
  ;; Delete the argument so that Dashboard setups its hooks normally.
  (setq command-line-args (delete switch command-line-args))
  ;; Set this variable if forced re-compilation is needed.
  (defvar zy-sync-force (equal switch "--force-sync")
    "If re-compilation is forced for this synchronization.")
  (load (expand-file-name "zy-sync.el" user-emacs-directory)
        nil 'nomessage 'nosuffix))

(defun zy-load-rel (relpath &rest args)
  "Load the file in relative path RELPATH.
RELPATH is relative to `user-emacs-directory', and is formatted
with ARGS like `format' does.

The file is always loaded as byte code. If not, issue an error.

This function is and should only be used in the initialization
file (init.el)."
  (let ((abspath (concat (expand-file-name
                          (apply #'format relpath args)
                          user-emacs-directory)
                         ".elc")))
    (condition-case nil
         ;; Announce the loading while debugging.
        (load abspath nil (not init-file-debug) 'nosuffix)
      (file-missing
       (error "%s not found. Maybe you forgot to sync?
Sync the configuration via \"emacs --sync\"" abspath)))))

(let (;; `file-name-handler-alist' is consulted each time a file is loaded.
      ;; Unsetting it speeds up startup notably.
      (file-name-handler-alist nil)
      ;; `load-source-file-function' decides what function to be called to do
      ;; code conversion before reading a source file. Since I do not need code
      ;; conversion while loading the configuration, unsetting it reduces tons
      ;; of startup time.
      (load-source-file-function nil)
      ;; Don't spend precious time checking modified time during startup.
      ;; Ensuring that the configuration is updated is the job of zy-sync.el.
      ;; Run "emacs --sync" to perform a synchronization of the configuration
      ;; before startup.
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
  (load custom-file nil (not init-file-debug) 'nosuffix)

  ;; Load all modules in order.
  (dolist (module zy-modules)
    (zy-load-rel "modules/zy-%s"
                 (substring (symbol-name module) 1))))

(provide 'init)

;;; init.el ends here
