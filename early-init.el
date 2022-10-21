;;; early-init.el --- Pre-initialization config -*- lexical-binding: t -*-

;;; Commentary:

;; This file is read before the GUI is initialized.

;;; Code:


;; Early settings

(setq default-frame-alist '(;; Maximize Emacs by default
			    (fullscreen . maximized)
			    ;; Default font for GUI
			    (font . "Fira Code-13")
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


(provide 'early-init)

;;; early-init.el ends here
