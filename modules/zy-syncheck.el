;;; zy-syncheck.el --- Syntax checking. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+syncheck' module of the configuration.

;; Flycheck is configured as the default syntax checker rather than the built-in
;; Flymake, because it is much more feature-rich and does not provide any
;; drawback. However, Eglot, the built-in LSP client, uses Flymake by default.
;; The Flycheck-eglot package overrides that behavior and make Flycheck work
;; with Eglot.

;;; Code:

(require 'zylib)

(pkg! 'flycheck)
(pkg! 'flycheck-inline)
(pkg! 'flycheck-eglot)

;; Enable Flycheck everywhere.
(add-hook! 'window-setup-hook (global-flycheck-mode 1))

;; Show diagnostics inline.
(add-hook! 'flycheck-mode-hook #'flycheck-inline-mode)

(daemon-require! 'flycheck)
(after! 'flycheck
  ;; Unset Flycheck's default echoing function, which breaks Eldoc.
  (setq flycheck-display-errors-function nil)

  ;; Don't show Flycheck markers. They are useless and don't work well with
  ;; other packages.
  (setq flycheck-indication-mode nil)

  ;; Inherit `load-path' from the running Emacs session while checking Emacs
  ;; Lisp code. This is useful while checking initialization code.
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Ignore package signatures during Emacs Lisp syntax checking. They are
  ;; irrelevant!
  (setq flycheck-emacs-lisp-package-initialize-form
        (eval-and-compile
          (format
           "%S"
           `(with-demoted-errors
                "Error during package initialization: %S"
              (setq package-check-signature nil)
              (package-initialize))))))

;; Use Flycheck rather than Flymake with Eglot.
(after! 'eglot
  (global-flycheck-eglot-mode 1))

(provide 'zy-syncheck)

;;; zy-syncheck.el ends here
