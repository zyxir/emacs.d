;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about the Chinese language.

;;; Code:

;; Built-in Rime input method.

(defvar *builtin-rime* nil
  "If built-in Rime is enabled.")
(unless (boundp 'rime-user-data-dir)
  (require 'cl-extra)
  (let* ((rime-data-possible-locs-win64
	  '("C:\\Users\\zyxir\\AppData\\Roaming\\Rime"))
	 (rime-data-possible-locs-linux
	  '("/mnt/c/Users/zyxir/AppData/Roaming/Rime"
	    "~/.emacs.d/rime"))
	 (guessed-rime-data
	  (cl-some
	   #'zy:file-directory-ret
	   (cond
	    (*win64* rime-data-possible-locs-win64)
	    (*linux* rime-data-possible-locs-linux)))))
    (if guessed-rime-data
	(progn
	  (setq rime-user-data-dir guessed-rime-data
		*builtin-rime* t)
	  (warn "Rime user data directory is auto detected."))
      (warn "No Rime user data is detected, so Rime is unavailable"))))
(when *builtin-rime*
  (use-package rime
    :straight t
    :config
    (setq default-input-method "rime"
	  rime-show-candidate 'posframe)))

;; Smart input source.
;; Platform-specific settings should be included in custom.el.

(use-package sis
  :straight t
  :config
  (sis-global-respect-mode t))

;; OpenCC.

(use-package opencc
  :commands (opencc-message
	     opencc-replace-at-point
	     opencc-print-buffer
	     opencc-insert-mode
	     opencc-isearch-mode)
  :straight
  (opencc :host github
	  :repo "zyxir/emacs-opencc"))

;; Lorem Ipsum Generator.

(use-package lorem-ipsum
  :straight t
  :commands
  (lorem-ipsum-insert-list
   lorem-ipsum-insert-sentences
   lorem-ipsum-insert-paragraphs))

;; End of config.

(provide 'init-lingua)
