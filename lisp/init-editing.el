;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Common text-editing features.

;;; Code:

;;;; For All Modes

;; Do not truncate lines by default.

(setq-default truncate-lines t)
(general-define-key "C-c $" #'toggle-truncate-lines)

;; Show matching parenthesis.

(use-package paren
  :defer 2
  :config
  (show-paren-mode +1))

;; Multiple cursors.

(use-package multiple-cursors
  :straight t
  :general
  ("C-c m" 'mc/edit-lines
   "M-<down-mouse-1>" nil
   "M-<mouse-1>" 'mc/add-cursor-on-click))

;; Set default fill column.

(setq-default fill-column 79)

;; Function to unfill paragraph.

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Insert some special characters.

(defun zy/insert-zero-width-space ()
  (interactive)
  (insert-char ?\u200B))
(general-define-key "C-x 8 s" #'zy/insert-zero-width-space)

;;;; For `prog-mode' and `text-mode'

;; Show and delete trailing whitespace.

(use-package emacs
  :config
  (defun zy/show-trailing-whitespace ()
    "Show trailing whitespace for the current buffer."
    (setq-local show-trailing-whitespace t))
  (add-hook 'prog-mode-hook 'zy/show-trailing-whitespace)
  (add-hook 'text-mode-hook 'zy/show-trailing-whitespace))

;; Snippet support.

(use-package yasnippet
  :straight t
  :general
  ("C-c s" 'yas-insert-snippet)
  :hook ((text-mode prog-mode) . yas-minor-mode)
  :delight yas-minor-mode)

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

;; Quick jump with avy.

(use-package avy
  :straight t
  :general
  ("M-r" 'avy-goto-char
   "M-t" 'avy-goto-char-timer))

;;;; For `prog-mode' Only

;; Show line numbers.

(add-hook 'prog-mode-hook
	  (lambda ()
	    (display-line-numbers-mode +1)))

;; Highlight indentation.

(use-package highlight-indent-guides
  :straight t
  :delight
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-responsive 'top))

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
  :delight
  :defer 2)

;; Language server protocol support.

(use-package lsp-mode
  :straight t
  :delight
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq read-process-output-max (* 1024 1024))
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
