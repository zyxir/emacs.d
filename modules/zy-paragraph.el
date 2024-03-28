;;; zy-paragraph.el --- Text paragraph. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+paragraph' module of the configuration.

;; Definitions like paragraphs, sentences, line-filling, indentation, and other
;; things which control the shape of the text are configured here.

;;; Code:

(require 'zylib)

(pkg! 'indent-bars
      :url "https://github.com/jdtsmith/indent-bars"
      :branch "main")

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

;; Display indent bars for all prog-modes except the Lisp modes.
(add-hook! 'prog-mode-hook
  (unless (derived-mode-p 'lisp-data-mode)
    (indent-bars-mode 1)))

(after! 'indent-bars
  ;; Use the "minimal colorpop" style for indent bars as instructed in the wiki.
  (setq-default
   indent-bars-color '(highlight :face-bg t :blend 0.15)
   indent-bars-pattern "."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   indent-bars-highlight-current-depth '(:blend 0.5)
   ;; Start from column 0, like VS Code does.
   indent-bars-starting-column 0
   ;; Do not show on blank lines, like VS Code does.
   indent-bars-display-on-blank-lines nil))

;; Display a `fill-column' indicator for prog modes.
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)

(add-hook! 'window-setup-hook
  ;; Always use subword movement (move between camel case words).
  (global-subword-mode 1)

  ;; Always enable visual line mode.
  (global-visual-line-mode 1))

(provide 'zy-paragraph)

;;; zy-paragraph.el ends here
