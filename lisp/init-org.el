;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; This file contains all configurations about org mode.

;;; Code:

;;;; Org Mode

(use-package org
  :hook
  (org-mode . auto-fill-mode)
  :general
  ("C-c C-i" nil
   "C-c c" 'org-capture
   "C-c a" 'org-agenda)
  :init
  ;; Allow Chinese around markups, maybe unstable, from
  ;; https://emacs-china.org/t/org-mode/597/4
  (setq org-emphasis-regexp-components
	;; markup 记号前后允许中文
	(list (concat " \t('\"{"            "[:nonascii:]")
	      (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
	      " \t\r\n,\"'"
	      "."
	      1))
  ;; Put attachments in an obvious directory.
  (setq org-attach-id-dir "org-attachments/"))

(provide 'init-org)
