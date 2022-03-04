;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Use Emacs as a backup PDF viewer.

;;; Code:

(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install))

;; End of config.

(provide 'init-pdf)
