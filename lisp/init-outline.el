;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about Outline minor mode.

;;; Code:

(use-package outline
  :hook ((emacs-lisp-mode
	   python-mode) . outline-minor-mode))

(use-package outshine
  :straight t
  :hook (outline-minor-mode . outshine-mode)
  :delight outshine-mode
  :init
  (defvar outline-minor-mode-prefix "\M-#"))

;; End of config.

(provide 'init-outline)
