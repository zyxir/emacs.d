;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about Python files.

;;; Code:

;; Use Pyright as default language server.

(use-package lsp-pyright
  :straight t
  :hook
  (python-mode .
	       (lambda ()
		 (require 'lsp-pyright)
		 (lsp))))

;; End of config.

(provide 'init-python)
