;;; early-init.el --- Pre-initialization config -*- lexical-binding: t -*-

;;; Commentary:

;; This file is read before the GUI is initialized.

;;; Code:


;; Speed up startup

(let ((normal-gc-cons-threshold (* 16 1024 1024))
      (normal-gc-cons-percentage gc-cons-percentage)
      (normal-file-name-handler-alist file-name-handler-alist)
      (init-gc-cons-threshold (* 128 1024 1024))
      (init-gc-cons-percentage 0.6)
      (init-file-name-handler-alist nil))
  (setq gc-cons-threshold init-gc-cons-threshold
	gc-cons-percentage init-gc-cons-percentage
	file-name-handler-alist init-file-name-handler-alist)
  (add-hook 'emacs-startup-hook
            (lambda ()
	      (setq gc-cons-threshold normal-gc-cons-threshold
		    gc-cons-percentage normal-gc-cons-percentage
		    file-name-handler-alist normal-file-name-handler-alist))))


;; Early settings

(setq default-frame-alist '(;; Maximize Emacs by default
			    (fullscreen . maximized)
			    ;; Default font for GUI
			    (font . "Fira Code-12")
			    ;; Disable menu bar.
                            (menu-bar-lines . nil)
                            ;; Disable scroll bars
                            (horizontal-scroll-bars . nil)
                            (vertical-scroll-bars . nil)
                            ;; Disable tool bar
                            (tool-bar-lines . 0))
      menu-bar-mode nil
      scroll-bar-mode nil
      tool-bar-mode nil
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      inhibit-startup-message t
      load-prefer-newer t
      package-enable-at-startup nil)

(require 'cl-lib)

(when (native-comp-available-p)
  (cl-eval-when 'compile
    (require 'comp))
  (setq native-comp-async-report-warnings-errors nil))


(provide 'early-init)

;;; early-init.el ends here
