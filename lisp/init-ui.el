;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary

;; UI related settings like tabs, side-bars, and window management.

;;; Code:

;;;; Window size

(setq default-frame-alist
      '((width . 100) (height . 46)))

;;;; Side bar

(use-package treemacs
  :straight t
  :after doom-themes
  :general
  ("M-0" 'treemacs-select-window)
  :init
  (setq treemacs-is-never-other-window t)
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
  ("C-x w" 'ace-window)
  :init
  (setq aw-scope 'frame
	aw-dispatch-always t)
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

;;;; Cursor indicator

(use-package beacon
  :straight t
  :delight
  :config
  (beacon-mode +1))

(provide 'init-ui)
