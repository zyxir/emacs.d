;;; zy-pdf.el --- PDF support. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+pdf' module of the configuration.

;; Emacs is not the best PDF viewer, but it is handy for previewing LaTeX. In
;; fact, that's the only use case for me.

;;; Code:

(require 'zylib)

(pkg! 'pdf-tools)

;; Load PDF Tools.
(when (fboundp 'pdf-loader-install)
  (pdf-loader-install))

(after! 'pdf-tools
  ;; Display PDF to fit page by default.
  (setq-default pdf-view-display-size 'fit-page))

;; HACK: Fix page blinking while Evil is on.
(add-hook! 'pdf-view-mode-hook
  (setq-local evil-normal-state-cursor (list nil)))

;; HACK: Silence "File *.pdf is large (X MiB), really open?" prompts for PDFs.
(advice-add
 #'abort-if-file-too-large :around
 (defun zy/pdf-suppress-large-file-prompts-a
     (fn size op-type filename &optional offer-raw)
   "Accept too large file for PDF files."
   (unless (string-match-p "\\.pdf\\'" filename)
     (funcall fn size op-type filename offer-raw))))

(provide 'zy-pdf)

;;; zy-pdf.el ends here
