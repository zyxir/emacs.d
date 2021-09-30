;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Settings about Markdown files.

;;; Code:

;; Pandoc is an optional dependency for preview.

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 ("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "pandoc -f markdown -t html5")
  :config
  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (setq truncate-lines nil)))
  (add-hook 'gfm-mode-hook
	    (lambda ()
	      (setq markdown-command "pandoc -f gfm -t html5"))))

;; TOC support.

(use-package markdown-toc
  :straight t
  :commands (markdown-toc-generate-or-refresh-toc)
  :general
  (:keymaps 'markdown-mode-command-map
	    "T" 'markdown-toc-generate-or-refresh-toc))

;; If vmd is installed, use it to live preview Markdown.

(defun zy:setup-vmd ()
  "Setup vmd-mode for markdown-mode."
  (when (executable-find "vmd")
    (use-package vmd-mode
      :straight t
      :general
      (:keymaps 'markdown-mode-command-map
		"p" 'vmd-mode))
    (remove-hook 'markdown-mode-hook #'zy:setup-vmd)))

(add-hook 'markdown-mode-hook #'zy:setup-vmd)

;; Enable C-c ' editing.

(use-package edit-indirect
  :straight t
  :after markdown-mode)

;; End of config.

(provide 'init-markdown)
