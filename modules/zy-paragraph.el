;;; zy-paragraph.el --- Text paragraph. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+paragraph' module of the configuration.

;; Definitions like paragraphs, sentences, line-filling, indentation, and other
;; things which control the shape of the text are configured here.

;;; Code:

(require 'zylib)

 ;; Use 80 characters as the global line length standard, as recommended by
 ;; the Google Style Guides.
(setq-default fill-column 80)

;; Most modern software use a single space after a sentence whereas Emacs uses
;; two by default. I prefer the modern style.
(setq-default sentence-end-double-space nil)

;; Use spaces instead of tabs by default.
(setq-default indent-tabs-mode nil)

;; Use four spaces for indentation by default.
(setq-default tab-width 4)

;; Allow line breaking after CJK characters. This is vital when displaying file
;; with CJK characters in `visual-line-mode'.
(setq word-wrap-by-category t)

;; Ability to unfill a paragraph.
(defun +paragraph/unfill-paragraph ()
  "Do the inverse of `fill-paragraph'."
  (interactive)
  (dlet ((fill-column most-positive-fixnum))
    (call-interactively 'fill-paragraph)))
(keybind! nil 'global "M-Q" #'+paragraph/unfill-paragraph)

;; Display a `fill-column' indicator for prog modes.
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)

(add-hook! 'window-setup-hook
  ;; Always use subword movement (move between camel case words).
  (global-subword-mode 1)

  ;; Always enable visual line mode.
  (global-visual-line-mode 1))

(provide 'zy-paragraph)

;;; zy-paragraph.el ends here
