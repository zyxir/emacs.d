;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about Outline minor mode.

;;; Code:

(use-package outline
  :delight outline-minor-mode
  :hook (prog-mode . outline-minor-mode))

;; End of config.

(provide 'init-outline)
