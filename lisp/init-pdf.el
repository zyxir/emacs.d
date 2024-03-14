;;; init-pdf.el --- PDF viewing.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'pdf-tools)

;; Load PDF Tools.
(pdf-loader-install)

(add-hook! pdf-view-mode
  (setq-local evil-normal-state-cursor (list nil)))

;; Silence "File *.pdf is large (X MiB), really open?" prompts for PDFs.
(defadvice! zy/pdf-suppress-large-file-prompts-a
  (fn size op-type filename &optional offer-raw)
  :around #'abort-if-file-too-large
  (unless (string-match-p "\\.pdf\\'" filename)
    (funcall fn size op-type filename offer-raw)))

(provide 'init-pdf)

;;; init-pdf.el ends here
