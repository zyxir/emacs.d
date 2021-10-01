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
  (:keymaps 'ctl-x-map
	    "C-r" 'counsel-recentf)
  :config
  (setq recentf-max-saved-items 200
	recentf-max-menu-items 15)
  (recentf-mode +1))

;; Revert file with hotkey.
;; However, auto-revert-mode is sometimes dangerous, see URL
;; `https://magit.vc/manual/magit/Risk-of-Reverting-Automatically.html#Risk-of-Reverting-Automatically'.

(general-define-key
 "C-c r"
 (lambda ()
   (interactive)
   (revert-buffer nil t)))

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

;; End of config.

(provide 'init-files)
