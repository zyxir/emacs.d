;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about LaTeX files.

;;; Code:

(use-package tex
  :defer t
  :straight auctex
  :config
  (add-to-list 'TeX-command-list '("XeLaTeX"
				   "%`xelatex%(mode)%' %t"
				   TeX-run-TeX
				   nil
				   t))
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-master nil
	TeX-show-compilation t))

;; End of config.

(provide 'init-latex)
