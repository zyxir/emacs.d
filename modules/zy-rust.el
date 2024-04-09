;;; zy-rust.el --- Rust development. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+rust' module of the configuration.

;; Rust is a great alternative to C/C++ with better documentation, more
;; standardized toolchain, and easier package management. The `rust-ts-mode' and
;; "rust-analyzer" language server already provide many features, and this
;; module just provides a little more.

;;; Code:

(require 'zylib)

(pkg! 'flycheck-rust)

(after! 'flycheck
  (add-hook! 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook! 'rust-ts-mode-hook
  (setq-local fill-column 100))

(provide 'zy-rust)

;;; zy-rust.el ends here
