;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about Python files.

;;; Code:

;; Use Pyright as default language server.

(use-package lsp-pyright
  :straight t
  :defer t
  :hook
  (python-mode .
	       (lambda ()
		 (require 'lsp-pyright)
		 (lsp))))

(use-package python
  :hook
  (python-mode . display-fill-column-indicator-mode))

;; End of config.

(provide 'init-python)
