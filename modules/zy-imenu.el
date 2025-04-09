;;; zy-imenu.el --- Imenu and enhancements -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+imenu' module of the configuration.

;; Imenu is a built-in facility that lists the major definitions in a code file.
;; This module installs and configures Imenu-list, which displays an
;; automatically updated buffer called *Ilist* to display Imenu entries.

;;; Code:

(require 'zylib)

(pkg! 'imenu-list)

(keybind! nil +leader-c-map
  "l" (cons "Ilist" #'imenu-list-smart-toggle))

(provide 'zy-imenu)

;;; zy-imenu.el ends here
