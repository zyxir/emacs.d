;;; -*- coding: utf-8; lexical-binding: t; -*-
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

(use-package general
  :straight t)

;; The keymap of my customized shortcuts.

;; Hide mode text with delight.

(use-package delight
  :straight t)

;; Minibuffer completion with the ivy suite.

(use-package ivy
  :straight t
  :defer 1
  :delight
  :general
  ("M-x" 'counsel-M-x
   "C-x C-f" 'counsel-find-file)
  :config
  (ivy-mode +1)
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t))

(use-package counsel
  :straight t
  :defer 1
  :after ivy
  :delight
  :config
  (counsel-mode +1))

(use-package swiper
  :straight t
  :after ivy
  :general
  ("C-s" 'swiper-isearch
   "C-r" 'swiper-isearch-backward))

;; End of config.

(provide 'init-mngt)
