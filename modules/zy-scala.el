;;; zy-scala.el --- Scala development. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+scala' module of the configuration.

;; Emacs has no built-in support for Scala. Therefore, I choose to only install
;; the tree-sitter version of Scala mode.

;;; Code:

(require 'zylib)

(pkg! 'scala-ts-mode)
(pkg! 'sbt-mode)

(add-hook! 'scala-ts-mode-hook
  (setq-local fill-column 100))

(provide 'zy-scala)

;;; zy-scala.el ends here
