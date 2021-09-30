;;; -*- lexical-binding: t; -*-
;;; Commentary:

;; File utilities.

;;; Code:

;; Save cursor position for visited files.

(use-package saveplace
  :defer 1
  :config
  (save-place-mode +1))

;; Save recent files.

(use-package recentf
  :defer 1
  :general
  ("C-x r" 'counsel-recentf)
  :config
  (setq recentf-max-saved-items 200
	recentf-max-menu-items 15)
  (recentf-mode +1))

;; Automatically revert files that changed on disk.

(use-package autorevert
  :defer 2
  :config
  (global-auto-revert-mode +1))

;; More reasonable unique buffer names.

(use-package uniquify
  :defer 2)

;; A more reasonable backup and auto save config.

(defvar zy/backup-directory nil
  "The location of backups.")
(setq zy/backup-directory (concat zy/emacs-d "backups"))
(unless (file-exists-p zy/backup-directory)
  (make-directory zy/backup-directory t))
(setq backup-directory-alist `(("." . ,zy/backup-directory)))
(setq make-backup-files t
      vc-make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 0
      kept-new-versions 10
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

;; Project management with projectile.

(use-package projectile
  :defer 1
  :straight t
  :delight '(:eval (concat " " (projectile-project-name)))
  :general
  (:keymaps 'ctl-x-map
	    "p" 'projectile-command-map)
  :config
  (projectile-mode +1))
