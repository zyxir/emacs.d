;;; early-init.el --- Emacs 27+ pre-initialization config.  -*- lexical-binding: t no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

;; Use a larger GC cons threshold to speed up startup. Once the GCMH package has
;; been loaded it will take over GC.
(setq gc-cons-threshold (* 1024 1024 1024))

;; Configure features here to speed up loading.
(setq
 ;; Turn off menu bar, scroll bars, and tool bar.
 default-frame-alist '(;; Disable menu bar.
                       (menu-bar-lines . nil)
                       ;; Disable scroll bars.
                       (horizontal-scroll-bars . nil)
                       (vertical-scroll-bars . nil)
                       ;; Disable tool bar.
                       (tool-bar-lines . 0)
                       ;; Set the font early.
                       (font . "-*-Sarasa Mono HC-*-*-*-*-16-*-*-*-*-*-*-1")
                       (x-compose-font-name)
                       ;; A reasonable default frame size.
                       (width . 110)
                       (height . 40))
 ;; These modes should also be `nil' to correspond to their actual states.
 menu-bar-mode nil
 scroll-bar-mode nil
 tool-bar-mode nil
 ;; Resize frames pixelwise instead of character-wise to prevent startup delay.
 frame-resize-pixelwise t
 ;; Disable package.el.
 package-enable-at-startup nil)

(provide 'early-init)

;;; early-init.el ends here
