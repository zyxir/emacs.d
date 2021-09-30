;;; -*- lexical-binding: t; -*-
;;; Commentary:

;; This file configures package management utilities.

;;; Code:

;; Make sure that straight.el is installed.

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Manage package and settings with use-package.

(straight-use-package 'use-package)

;; Manage keybindings with general.

(use-package general
  :straight t)

;; Hide mode text with delight.

(use-package delight
  :straight t)

;; Minibuffer completion with ivy and counsel.

(use-package ivy
  :straight t
  :delight
  :config
  (ivy-mode +1)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t))

(use-package counsel
  :straight t
  :after ivy
  :delight
  :config
  (counsel-mode +1))

(provide 'init-mngt)
