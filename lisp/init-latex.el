;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about LaTeX files.

;;; Code:

(use-package lsp-latex
  :straight t
  :defer t
  :general
  (:keymaps 'latex-mode-map
	    "C-c l b" 'lsp-latex-build)
  :hook
  ((latex-mode yatex-mode bibtex-mode) .
   (lambda ()
     (auto-fill-mode +1)
     (display-line-numbers-mode +1)
     (lsp))))

;; End of config.

(provide 'init-latex)
