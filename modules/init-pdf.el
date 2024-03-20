;;; init-pdf.el --- PDF viewing.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(pkg! 'pdf-tools)

;; Load PDF Tools.
(when (fboundp 'pdf-loader-install)
  (pdf-loader-install))

(after! 'pdf-tools
  ;; Display PDF to fit page by default.
  (setq-default pdf-view-display-size 'fit-page))

;; HACK Fix page blinking while Evil is on.
(add-hook! pdf-view-mode
  (setq-local evil-normal-state-cursor (list nil)))

;; HACK Silence "File *.pdf is large (X MiB), really open?" prompts for PDFs.
(defadvice! zy/pdf-suppress-large-file-prompts-a
  (fn size op-type filename &optional offer-raw)
  :around #'abort-if-file-too-large
  (unless (string-match-p "\\.pdf\\'" filename)
    (funcall fn size op-type filename offer-raw)))

(provide 'init-pdf)

;;; init-pdf.el ends here
