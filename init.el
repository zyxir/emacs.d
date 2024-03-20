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
(defconst zy-modules '(;; Setup Evil and the leader key.
                       evil
                       leader
                       ;; Text-editing and coding.
                       quickins)
  "Enabled modules of Zyxir's Emacs configuration.")

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
      ;; Don't spend precious time checking modified time during startup.
      ;; TODO: mention a way to sync the configuration.
      (load-prefer-newer nil))

  ;; Load all components of Zylib manually, which defines utility functions and
  ;; macros that most parts of this configuration depend on. Load them by path
  ;; manually reduces the tiny bit of time used to locate them in `load-path'.
  (zy-load-rel "lisp/zylib-core")
  (zy-load-rel "lisp/zylib-pkg")
  (zy-load-rel "lisp/zylib-key")
  (zy-load-rel "lisp/zylib")

  ;; Load all modules in order.
  (dolist (module zy-modules)
    (zy-load-rel "modules/zy-%s" module)))

;;;; Load modules.

(defvar zy/modules-dir (expand-file-name "modules" user-emacs-directory)
  "Directory containing modules of Zyxir's Emacs configuration.")

(add-to-list 'load-path zy/modules-dir)

(defvar zy/modules '()
  "All modules of Zyxir's Emacs configuration.
They are placed in a reverse chronological order.")

(defun zy/compile-file (file)
  "Compile FILE in all ways possible.
MODULE-FILE is a `.el' file. This function byte-compile it, and
natively compile it asynchronously, if native compilation is
available."
  ;; Byte compile the file, and ignore warnings while doing it, since all actual
  ;; warnings should be eliminated while writting the code, and only the
  ;; compiled byte code matters to the user (for example, Evil will warn that
  ;; some variable is set after Evil is loaded, but Evil is only loaded early in
  ;; compile time in order to supress warnings in `eval-after-load' blocks,
  ;; therefore the warning does not matter at runtime).
  (defvar byte-compile-warnings)
  (let ((byte-compile-warnings nil))
    (byte-compile-file file))
  ;; Additionally, if native compilation is available, native-compile the
  ;; byte code.
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (native-compile-async file nil 'load)))

(defun zy/require-init (module)
  "Load module MODULE of the configuration.
Always try to load the compiled version of MODULE (if not
compiled yet, compile it now).

Compilation optimizes load speed and ensures proper loading
order (otherwise some features might be loaded early by macro
expansion).

Yes, compiling the init file might introduce more issues than it
solves. However, as a perfectionist, I prefer eliminating all
Flycheck warnings of my config, since a warning might stand for a
potential bug. I am willing to invest time to ensure the byte
code compatibility of my config."
  (let* ((base (expand-file-name (symbol-name module) zy/modules-dir))
         (source (concat base ".el"))
         (compiled (concat base ".elc"))
         (outdated (file-newer-than-file-p source compiled)))
    (if t
        ;; Currently this branch will never be executed. It is left for
        ;; convenience.
        (load source 'noerror 'nomessage 'nosuffix)
      ;; Compile the source file if the byte code is outdated.
      (when outdated (zy/compile-file source))
      ;; Load the compiled byte code.
      (load compiled 'noerror 'nomessage 'nosuffix))
    ;; Push the module to the list.
    (setq zy/modules (cons module zy/modules))))

(defun zy/compile-all ()
  "Re-compile all modules of Zyxir's Emacs configuration.
A clean re-compilation helps reduce errors introduced by byte
code inconsistency."
  (interactive)
  (dolist (module (reverse zy/modules))
    (let* ((file (expand-file-name (concat (symbol-name module) ".el")
                                   zy/modules-dir)))
      (zy/compile-file file))))
(defalias 'zy/recompile-all #'zy/compile-all)

;; Clear the file name handler to make loading faster.
;; (let* ((file-name-handler-alist nil))

;;   ;; Load the custom file first.
;;   (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;   (load custom-file 'noerror 'nomessage)

;;   ;; Basic modules.
;;   ;; (zy/require-init 'init-elpa)
;;   ;; (zy/require-init 'init-util)
;;   ;; (zy/require-init 'init-keybindings)

;;   ;; A placeholder module which ensures every basic module has been loaded.
;;   (zy/require-init 'init-basic)

;;   ;; Applications and features.
;;   (zy/require-init 'init-personal)
;;   (zy/require-init 'init-dired)
;;   (zy/require-init 'init-lingual)
;;   (zy/require-init 'init-os)

;;   ;; Text-editing and coding.
;;   (zy/require-init 'init-misc)
;;   (zy/require-init 'init-paragraph)
;;   (zy/require-init 'init-snippet)
;;   (zy/require-init 'init-completion)
;;   (zy/require-init 'init-search)
;;   (zy/require-init 'init-vc)
;;   (zy/require-init 'init-treesit)
;;   (zy/require-init 'init-project)
;;   (zy/require-init 'init-highlight)
;;   (zy/require-init 'init-check)
;;   (zy/require-init 'init-lsp)
;;   (zy/require-init 'init-env)
;;   (zy/require-init 'init-prose)

;;   ;; Look and feel.
;;   (zy/require-init 'init-theme)
;;   (zy/require-init 'init-modeline)
;;   (zy/require-init 'init-fonts)

;;   ;; File type specific.
;;   (zy/require-init 'init-lisp)
;;   (zy/require-init 'init-nix)
;;   (zy/require-init 'init-org)
;;   (zy/require-init 'init-pdf)
;;   (zy/require-init 'init-python)
;;   (zy/require-init 'init-scala)
;;   (zy/require-init 'init-tex)
;;   (zy/require-init 'init-other-modes))

(provide 'init)

;;; init.el ends here
