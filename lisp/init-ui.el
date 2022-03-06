;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary

;; UI related settings like tabs, side-bars, and window management.

;;; Code:

;;;; Frame Settings

;; Default frame name.
(setq frame-title-format
      '("" "emacs" " [%b]"))

;; Default frame size.
(defvar zy/default-frame-width 86
  "Default width of frames, in characters.")
(defvar zy/default-frame-height 40
  "Default height of frames, in characters.")

;; Set frame to default size.
(defun zy/frame-resize-to-default ()
  "Set frame to default size."
  (interactive)
  (set-frame-width (selected-frame) zy/default-frame-width)
  (set-frame-height (selected-frame) zy/default-frame-height))
(general-define-key "C-c f" #'zy/frame-resize-to-default)

;; Default frame parameters.
(defun zy/frame-func (frame)
  "Function to run after a frame is created."
  (select-frame frame)
  ;; Set font.
  (zy/set-font)
  ;; Set frame parameters.
  (scroll-bar-mode -1)
  (set-frame-height frame zy/default-frame-height)
  ;; Load theme again.
  (load-theme zy/default-theme t))
(add-hook 'after-make-frame-functions #'zy/frame-func)
(setq default-frame-alist
      `((height . ,zy/default-frame-height)
	(width . ,zy/default-frame-width)))

;;;; Modeline

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project
	doom-modeline-minor-modes t
	doom-modeline-lsp t))

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
  :commands treemacs-projectile
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
