;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about the Chinese language.

;;; Code:

;; Built-in Rime input method.

(use-package rime
  :straight t
  :general
  ("C-`" 'rime-send-keybinding)
  ("M-m" 'rime-inline-ascii)
  :config
  (setq rime-user-data-dir (concat user-emacs-directory "rime")
	default-input-method "rime"
	rime-show-candidate 'posframe))

;; OpenCC.

(use-package opencc
  :commands (opencc-message
	     opencc-replace-at-point
	     opencc-print-buffer
	     opencc-insert-mode
	     opencc-isearch-mode)
  :straight
  (opencc :host github
	  :repo "zyxir/emacs-opencc"))

;; Lorem Ipsum Generator.

(use-package lorem-ipsum
  :straight t
  :commands
  (lorem-ipsum-insert-list
   lorem-ipsum-insert-sentences
   lorem-ipsum-insert-paragraphs))

;; End of config.

(provide 'init-lingua)
