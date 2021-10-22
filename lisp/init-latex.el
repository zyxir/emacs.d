;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about LaTeX files.

;;; Code:

(use-package lsp-latex
  :straight t
  :after lsp-mode
  :hook
  ((latex-mode yatex-mode bibtex-mode) .
   (lambda ()
     (display-line-numbers-mode +1)
     (lsp))))

(use-package latex
  :after lsp-latex
  :general
  (:keymaps 'latex-mode-map
	    "C-c l b" 'lsp-latex-build))

;; End of config.

(provide 'init-latex)
