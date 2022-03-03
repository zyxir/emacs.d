;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about the Chinese language.

;;; Code:

;; Built-in Rime input method.

(use-package rime
  :straight t
  :config
  (setq rime-user-data-dir (concat user-emacs-directory "rime")
	default-input-method "rime"
	rime-show-candidate 'posframe)
  ;; Change cursor color to orange if IM is active.
  (defvar zy/im-cursor-color "Orange"
    "Default cursor color if an input method is active.")
  (defvar zy/default-cursor-color (frame-parameter nil 'cursor-color)
    "Default text cursor color.")
  (defun zy/change-cursor-color-on-im ()
    "Set cursor color depending IM state."
    (interactive)
    (set-cursor-color (if current-input-method
			  zy/im-cursor-color
			zy/default-cursor-color)))
  (add-hook 'post-command-hook 'zy/change-cursor-color-on-im))

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
