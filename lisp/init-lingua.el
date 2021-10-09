;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about the Chinese language.

;;; Code:

;; Smart input source.
;; Platform-specific settings should be included in custom.el.

(use-package sis
  :straight t
  :config
  (sis-global-respect-mode t))

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
  :straight t)

;; End of config.

(provide 'init-chinese)
