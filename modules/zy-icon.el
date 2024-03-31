;;; zy-icon.el --- Icon support. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+icon' module of the configuration.

;; Provide icon support for various features via Nerd Fonts.

;;; Code:

(require 'zylib)

(pkg! 'nerd-icons)
(pkg! 'nerd-icons-dired)
(pkg! 'nerd-icons-completion)
(pkg! 'nerd-icons-corfu)
(pkg! 'treemacs-nerd-icons)

;; Icon support for Dired.
(add-hook! 'dired-mode-hook
  (nerd-icons-dired-mode 1))

;; Icon support for Corfu.
(after! 'corfu
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Icon support for Treemacs.
(after! 'treemacs
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))

(provide 'zy-icon)

;;; zy-icon.el ends here
