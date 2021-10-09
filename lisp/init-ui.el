;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary

;; UI related settings like tabs, side-bars, and window management.

;;; Code:

;;;; Side bar

(use-package treemacs
  :straight t
  :after doom-themes
  :general
  ("M-0" 'treemacs-select-window)
  :config
  ;; Use doom theming.
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

;;;; Window numbering

(use-package ace-window
  :straight t
  :general
  ("C-x o" 'ace-window)
  :init
  (setq aw-scope 'frame)
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

;;;; Cursor indicator

(use-package beacon
  :straight t
  :config
  (beacon-mode +1))

(provide 'init-ui)
