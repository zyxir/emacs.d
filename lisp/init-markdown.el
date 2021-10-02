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
  (add-hook 'gfm-mode-hook
	    (lambda ()
	      (setq markdown-command "pandoc -f gfm -t html5"))))

;; Live preview Github Flavored Markdown with vmd.
;; This makes vmd as an optional dependency.

(defun zy:setup-vmd ()
  "Setup vmd-mode if vmd exists."
  (when (executable-find "vmd")
    (use-package vmd-mode
      :straight
      (vmd-mode :type git :host github :repo "blak3mill3r/vmd-mode")
      :commands vmd-mode
      :general
      (:keymaps 'gfm-mode-map
		"C-c C-c p" 'vmd-mode))
    (remove-hook 'gfm-mode-hook #'zy:setup-vmd)))

(add-hook 'gfm-mode-hook #'zy:setup-vmd)

;; Enable C-c ' editing.

(use-package edit-indirect
  :straight t
  :after markdown-mode)

;; End of config.

(provide 'init-markdown)
