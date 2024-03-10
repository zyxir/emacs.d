;;; init-paragraph.el --- Paragraph settings.  -*- lexical-binding: t -*-
;;; Commentary:

;; Settings about sentences, lines, indentation, and paragraphs.

;;; Code:

(require-vc-package '(indent-bars
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
(bind-key "M-Q" #'zy/unfill-paragraph)

;; Display a `fill-column' indicator.
(add-hook! prog-mode #'display-fill-column-indicator-mode)

(provide 'init-paragraph)

;;; init-paragraph.el ends here
