;;; -*- coding: utf-8; lexical-binding: t; -*-
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
  (let ((inhibit-message t))
    (recentf-mode +1)))

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

;; Do not create backup files, as version control is common these days.

(setq make-backup-files nil)

;; Replace built-in autosave with super-save.

(setq auto-save-default nil)
(use-package super-save
  :straight t
  :defer 2
  :delight
  :config
  (setq super-save-auto-save-when-idle t)
  (super-save-mode +1))

;; Project management with projectile.

(use-package projectile
  :defer 2
  :straight t
  :delight '(:eval (concat " " (projectile-project-name)))
  :general
  (:keymaps 'ctl-x-map
	    "p" 'projectile-command-map)
  :config
  (projectile-mode +1))

;; End of config.

(provide 'init-files)
