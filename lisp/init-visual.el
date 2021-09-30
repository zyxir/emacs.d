;;; -*- lexical-binding: t; -*-
;;; Commentary:

;; Visual configurations like themes and fonts.

;;; Code:

;; Install and enable nano theme.

(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "404cn/nano-theme.el")
  :config
  (setq nano-theme-light/dark 'light
	nano-theme-comment-italic t
	nano-theme-keyword-italic nil
	nano-theme-padded-modeline 2
	nano-theme-overline-modeline nil)
  (load-theme 'nano t))

;; Solaire mode.

(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))
