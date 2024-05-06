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
(pkg! 'cargo)

(after! 'flycheck
  (add-hook! 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook! 'rust-ts-mode-hook
  (setq-local fill-column 100)

  (eval-and-compile (require 'cargo))

  (after! '+leader
    (defprefix! +rust-map "Rust"
                nil rust-ts-mode-map "<localleader>"
      "c" (cons "Clean" #'cargo-process-clean)
      "r" (cons "Run" #'cargo-process-run))
    (defprefix! +rust-test-map "Test"
                nil +rust-map "t"
      "t" (cons "Test" #'cargo-process-test)
      "f" (cons "File" #'cargo-process-current-file-tests)
      "c" (cons "Current" #'cargo-process-current-test))))

(provide 'zy-rust)

;;; zy-rust.el ends here
