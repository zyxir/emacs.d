;;; zy-syncheck.el --- Syntax checking. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+syncheck' module of the configuration.

;; Flycheck is configured as the default syntax checker rather than the built-in
;; Flymake, because it is much more feature-rich and does not provide any
;; drawback.
;;
;; However, Eglot, the built-in LSP client, uses Flymake by default, for its
;; seemingly better design and standard protocols. These configurations try to
;; minimize the difference of these syntax checkers and provide a consistent
;; experience.

;;; Code:

(require 'zylib)

(pkg! 'flycheck)

;; Enable Flycheck for modes without Eglot support.
(add-hook! 'emacs-lisp-mode-hook
  (flycheck-mode 1))

(daemon-require! 'flycheck)
(after! 'flycheck
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

(daemon-require! 'flymake)
(after! 'flymake
  ;; Don't show fringe indicators like Flycheck does.
  (setq flymake-fringe-indicator-position nil))

(provide 'zy-syncheck)

;;; zy-syncheck.el ends here
