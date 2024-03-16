;;; init-paragraph.el --- Paragraph settings.  -*- lexical-binding: t -*-
;;; Commentary:

;; Settings about sentences, lines, indentation, and paragraphs.

;;; Code:

(require 'init-keybindings)

(require-package '(indent-bars
                   :url "https://github.com/jdtsmith/indent-bars"))

(setq-default
 ;; Use 80 characters as the global line length standard (recommended by
 ;; Google).
 fill-column 80
 ;; Most modern software use a single space after a sentence whereas Emacs uses
 ;; two by default. I prefer the modern style.
 sentence-end-double-space nil
 ;; Use spaces instead of tabs by default.
 indent-tabs-mode nil
 ;; Use four spaces for indentation by default.
 tab-width 4)

;; Ability to unfill a paragraph.
(defun zy/unfill-paragraph ()
  "Do the inverse of `fill-paragraph'."
  (interactive)
  (dlet ((fill-column most-positive-fixnum))
    (call-interactively 'fill-paragraph)))
(general-def "M-Q" #'zy/unfill-paragraph)

;; Customize indent bars.
(setq-default
 ;; Minimal colorpop style.
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
 indent-bars-display-on-blank-lines nil)
;; Display indent bars for all prog-modes except the Lisp modes.
(add-hook! prog-mode
  (unless (derived-mode-p 'lisp-data-mode)
    (indent-bars-mode 1)))

;; Display a `fill-column' indicator.
(add-hook! prog-mode #'display-fill-column-indicator-mode)

;; Always show a button when using outline.
(after! 'outline
  (setq outline-minor-mode-use-buttons t))

;; Always enable visual line mode.
(global-visual-line-mode 1)

(provide 'init-paragraph)

;;; init-paragraph.el ends here
