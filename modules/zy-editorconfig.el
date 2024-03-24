;;; zy-editorconfig.el --- Respect EditorConfig. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+editorconfig' module of the configuration.

;; EditorConfig is a file format for maintaining consistent styles for multiple
;; developers working on the same project across various editors and IDEs.
;; Support for EditorConfig can be enabled by enabling a simple package.

;;; Code:

(require 'zylib)

(pkg! 'editorconfig)

(add-hook! 'window-setup-hook
  (editorconfig-mode 1))

(provide 'zy-editorconfig)

;;; zy-editorconfig.el ends here
