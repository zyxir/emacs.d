;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; This file configures package management utilities.

;;; Code:

;;;; Package management

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
(setq straight-vc-git-default-clone-depth 1)

;; Hide mode text with delight.

(use-package delight
  :straight t)

;;;; Keybinding management

;; Shortcut hint with which-key.

(use-package which-key
  :straight t
  :defer 2
  :delight
  :config
  ;; Popup side window on bottom.
  (which-key-setup-side-window-bottom)
  ;; Show which-key on C-h.
  (setq which-key-show-early-on-C-h t)
  (which-key-mode +1))

;; Manage shortcuts with general.

(use-package general
  :straight t)

;;;; Minibuffer

;; Minibuffer completion with the ivy suite.

(use-package ivy
  :straight t
  :delight
  :init
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :config
  (ivy-mode +1))

(use-package counsel
  :straight t
  :after ivy
  :delight
  :general
  ("M-s g" 'counsel-rg)
  :config
  (counsel-mode +1))

;; Install smex, which will be automatically used by counsel-M-x.

(use-package smex
  :straight t)

(use-package swiper
  :straight t
  :after ivy
  :general
  ("C-s" 'swiper-isearch
   "C-r" 'swiper-isearch-backward))

;; Command map for tweakering functions.

(define-prefix-command 'zy/tweakering-map)
(general-define-key "C-c \\" 'zy/tweakering-map)
(general-define-key
 :keymaps 'zy/tweakering-map
 "l" 'zy/load-times)

;; Restart Emacs from within Emacs.

(use-package restart-emacs
  :straight t
  :general
  (:keymaps 'zy/tweakering-map
	    "R" 'restart-emacs
	    "N" 'restart-emacs-start-new-emacs)
  :commands (restart-emacs
	     restart-emacs-start-new-emacs))

;; End of config.

(provide 'init-mngt)
