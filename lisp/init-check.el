;;; init-check.el --- Syntax and spell checking.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'flycheck)
(require-package 'flycheck-eglot)

;; Enable Flycheck everywhere.
(global-flycheck-mode 1)

;; Use Flycheck rather than Flymake with Eglot.
(with-eval-after-load 'eglot
  (global-flycheck-eglot-mode 1))

(provide 'init-check)

;;; init-check.el ends here
