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

;; Flycheck as syntax checking framework.

(use-package flycheck
  :straight t
  :defer 2)

;; End of config.

(provide 'init-editing)
