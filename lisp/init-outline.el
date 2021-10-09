;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about Outline minor mode.

;;; Code:

(use-package outshine
  :straight t
  :hook ((prog-mode . outline-minor-mode)
         (outline-minor-mode . outshine-mode))
  :delight outshine-mode
  :init
  (defvar outline-minor-mode-prefix "\M-#"))

(use-package outline
  :delight outline-minor-mode)

;; End of config.

(provide 'init-outline)
