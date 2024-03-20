;;; early-init.el --- Emacs 27+ pre-initialization config.  -*- lexical-binding: t no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

;; Garbage collection reduces startup time. This line inhibits GC during
;; startup. Once the GCMH package is loaded it will take over GC.
(setq gc-cons-threshold most-positive-fixnum)

;; Don't check modified time on Emacs Lisp byte code during an interactive
;; startup. TODO: mention a way to recompile the config.
(setq load-prefer-newer noninteractive)

;; Respect the DEBUG environment variable as an alternative to "--debug-init".
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

;; Turn off the automatic call to `package-initialize' so that we can decide
;; when exactly package.el is loaded.
(setq package-enable-at-startup nil)

;; Configure GUI features here to speed up loading. It will be much more
;; expensive to configure the GUI once it has been started, as the frame may be
;; resized and redrawn multiple times.
(setq
 ;; Turn off menu bar, scroll bars, and tool bar.
 default-frame-alist '(;; Disable menu bar.
                       (menu-bar-lines . nil)
                       ;; Disable scroll bars.
                       (horizontal-scroll-bars . nil)
                       (vertical-scroll-bars . nil)
                       ;; Disable tool bar.
                       (tool-bar-lines . 0)
                       ;; A reasonable default frame size.
                       (width . 110)
                       (height . 40))
 ;; These modes should also be `nil' to correspond to their actual states.
 menu-bar-mode nil
 scroll-bar-mode nil
 tool-bar-mode nil
 ;; Resize frames pixelwise instead of character-wise to prevent startup delay.
 frame-resize-pixelwise t)

(provide 'early-init)

;;; early-init.el ends here
