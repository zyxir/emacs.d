;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about Outline minor mode.

;;; Code:

(use-package outshine
  :straight t
  :hook (prog-mode . outshine-mode)
  :init
  (setq outline-minor-mode-prefix "\M-#"))

;; End of config.

(provide 'init-outline)
