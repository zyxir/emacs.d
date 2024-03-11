;;; init-check.el --- Syntax and spell checking.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'flycheck)

;; Enable Flycheck everywhere.
(global-flycheck-mode 1)

;; Jump between errors.
(general-def
  :states 'normal
  "]q" 'flycheck-next-error
  "[q" 'flycheck-previous-error
  "[Q" 'flycheck-first-error
  "]Q" 'flycheck-list-errors)

(provide 'init-check)

;;; init-check.el ends here
