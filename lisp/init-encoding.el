;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about encoding.

;;; Code:

;; Set most things to UTF-8, preferably the Unix variation..

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use Unicad to decide file coding system.

(use-package unicad
  :straight t
  :delight
  :config
  (unicad-mode +1))

;; End of config.

(provide 'init-encoding)
