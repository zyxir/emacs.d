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
  "Load module MODULE of the configuration."
  (let* ((base (expand-file-name (symbol-name module) zy/module-dir))
         (source (concat base ".el"))
         (compiled (concat base ".elc"))
         (should-compile (file-newer-than-file-p source compiled)))
    (when should-compile
      (byte-compile-file source)
      (when (and (fboundp 'native-comp-available-p)
                 (native-comp-available-p))
        (native-compile-async compiled nil 'load)))
    (load compiled 'noerror 'nomessage 'nosuffix)))

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
