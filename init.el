;;; init.el --- Boostrap config modules.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;; Preparations

;; Check Emacs version.
(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Emacs %s or higher is required to run Zyxir's config" minver)))

;;;; Load modules.

(defvar zy/module-dir (expand-file-name "lisp" user-emacs-directory)
  "Directory containing modules of Zyxir's Emacs configuration.")

(add-to-list 'load-path zy/module-dir)

(defun require-init (module)
  "Load module MODULE of the configuration.
Always try to load the compiled version of MODULE (if not
compiled yet, compile it now), unless `init-file-debug' is
non-nil. In that case, load the uncompiled version with \".el\"
suffix.

Compilation optimizes load speed and ensures proper loading
order (otherwise some features might be loaded early by macro
expansion).

Yes, compiling the init file might introduce more issues than it
solves. However, as a perfectionist, I prefer eliminating all
Flycheck warnings of my config, since a warning might stand for a
potential bug. I am willing to invest time to ensure the byte
code compatibility of my config."
  (let* ((base (expand-file-name (symbol-name module) zy/module-dir))
         (source (concat base ".el"))
         (compiled (concat base ".elc"))
         (outdated (file-newer-than-file-p source compiled)))
    (if init-file-debug
        ;; Load the uncompiled version if `init-file-debug' is non-nil.
        (load source 'noerror 'nomessage 'nosuffix)
      (when outdated
        ;; Byte compile the file, and ignore warnings while doing it, since all
        ;; actual warnings should be eliminated while writting the code, and
        ;; only the compiled byte code matters to the user (for example, Evil
        ;; will warn that some variable is set after Evil is loaded, but Evil is
        ;; only loaded early in compile time in order to supress warnings in
        ;; `eval-after-load' blocks, therefore the warning does not matter at
        ;; runtime).
        (let ((byte-compile-warnings nil))
          (byte-compile-file source))
        ;; Additionally, if native compilation is available, native-compile the
        ;; byte code.
        (when (and (fboundp 'native-comp-available-p)
                   (native-comp-available-p))
          (native-compile-async source nil 'load)))
      ;; Load the compiled byte code.
      (load compiled 'noerror 'nomessage 'nosuffix))))

;; Clear the file name handler to make loading faster.
(let* ((file-name-handler-alist nil))

  ;; Load the custom file first.
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror 'nomessage)

  ;; Basic modules.
  (require-init 'init-elpa)
  (require-init 'init-util)
  (require-init 'init-keybindings)

  ;; A placeholder module which ensures every basic module has been loaded.
  (require-init 'init-basic)

  ;; Text-editing and coding.
  (require-init 'init-misc)
  (require-init 'init-paragraph)
  (require-init 'init-snippet)
  (require-init 'init-completion)
  (require-init 'init-search)
  (require-init 'init-vc)
  (require-init 'init-treesit)
  (require-init 'init-project)
  (require-init 'init-highlight)
  (require-init 'init-check)
  (require-init 'init-lsp)
  (require-init 'init-env)

  ;; Look and feel.
  (require-init 'init-theme)
  (require-init 'init-modeline)
  (require-init 'init-fonts)

  ;; File type specific.
  (require-init 'init-lisp)
  (require-init 'init-nix)
  (require-init 'init-pdf)
  (require-init 'init-python)
  (require-init 'init-scala)
  (require-init 'init-other-modes))

(provide 'init)

;;; init.el ends here
