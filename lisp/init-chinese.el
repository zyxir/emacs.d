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

;; End of config.

(provide 'init-chinese)
