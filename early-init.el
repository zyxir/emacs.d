;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; This file is loaded early in Emacs 27 for better performance.

;;; Code:

;; Do not enable packages early.

(setq package-enable-at-startup nil)

;; Modify garbage collection at startup and after it.
;; URL `https://www.reddit.com/r/emacs/comments/ofhket/further_boost_start_up_time_with_a_simple_tweak/'

(defvar tmp/file-name-handler-alist file-name-handler-alist
  "Temporary storer for file-name-handler-alist at startup.")
(setq gc-cons-threshold 536870921	; 512 MB
      gc-cons-percentage 0.6
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 67108864 ; 64 MB
		  gc-cons-percentage 0.1
		  file-name-handler-alist tmp/file-name-handler-alist)))

;; Turn off startup screen, and tune other UI settings.

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)

;; A reasonable default GUI size.

(when window-system
  (setq default-frame-alist '((width . 120) (height . 40))))

;; Disable built-in vc. This makes config file loading a bit faster.

(setq vc-handled-backends nil)

;; End of config.

(provide 'early-init)
