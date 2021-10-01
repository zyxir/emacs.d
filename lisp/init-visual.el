;;; -*- lexical-binding: t; -*-
;;; Commentary:

;; Visual configurations like themes and fonts.

;;; Code:

;; Install and enable spacemacs theme.

(use-package spacemacs-common
  :straight spacemacs-theme
  :config
  (load-theme 'spacemacs-light t))

;; Solaire mode.

(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))

;; End of config.

(provide 'init-visual)
