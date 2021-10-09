;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Common text-editing features.

;;; Code:

;;;; For All Modes

;; Show matching parenthesis.

(use-package paren
  :defer 2
  :config
  (show-paren-mode +1))

;;;; For `prog-mode' and `text-mode'

;; Show and delete trailing whitespace.

(use-package emacs
  :general
  ("C-c SPC" 'delete-trailing-whitespace)
  :config
  (defun zy/show-trailing-whitespace ()
    "Show trailing whitespace for the current buffer."
    (setq-local show-trailing-whitespace t))
  (add-hook 'prog-mode-hook 'zy/show-trailing-whitespace)
  (add-hook 'text-mode-hook 'zy/show-trailing-whitespace)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;;; For `prog-mode' Only

;; Set default fill column.

(setq-default fill-column 79)

;; Show line numbers.

(add-hook 'prog-mode-hook
	  (lambda ()
	    (display-line-numbers-mode +1)))

;; Make Emacs aware of camel case.

(use-package subword
  :hook (prog-mode-hook . subword-mode)
  :delight)

;; Company as code completion framework.

(use-package company
  :straight t
  :defer 1
  :delight
  :config
  (add-hook 'prog-mode-hook
	    (lambda ()
	       (company-mode +1)
	       (general-define-key
		:keymaps 'local
		"C-M-i" 'company-complete))))

;; Use LSP as company backend.

(use-package company-lsp
  :straight t
  :after (company lsp-mode)
  :config
  (push 'company-lsp company-backends))

;; Flycheck as syntax checking framework.

(use-package flycheck
  :straight t
  :defer 2)

;; Language server protocol support.

(use-package lsp-mode
  :straight t
  :delight
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :commands lsp-ui-mode
  :general
  (:keymap 'lsp-ui-mode-map
	   "M-." #'lsp-ui-peek-find-definitions
	   "M-?" #'lsp-ui-peek-find-references))

(use-package lsp-ivy
  :straight t
  :after (lsp-mode ivy)
  :commands lsp-ivy-workspace-symbol)

;; End of config.

(provide 'init-editing)
