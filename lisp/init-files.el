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

(use-package autorevert
  :delight auto-revert-mode
  :general
  ("C-c r"
   (lambda ()
     (interactive)
     (revert-buffer nil t))))

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
  :straight t
  :delight
  :general
  ("C-x p f" 'projectile-find-file
   "C-x p d" 'projectile-find-dir
   "C-x p p" 'projectile-switch-project)
  ;; Only these two functions are auto loaded.
  :config
  (when (boundp 'zy/projects-path)
    (add-to-list 'projectile-project-search-path zy/projects-path))
  (add-to-list 'projectile-project-search-path zy/emacs-d)
  (setq projectile-sort-order 'recently-active)
  (projectile-mode +1)
  (general-define-key "C-x p" 'projectile-command-map))

;; End of config.

(provide 'init-file)
