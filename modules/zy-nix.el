;;; zy-nix.el --- Nix development. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+nix' module of the configuration.

;; Nix is both a package manager, a language, and an operating system. This
;; module mainly provides support for the Nix language, but can also contain
;; configurations for the other two.

;;; Code:

(require 'zylib)

(pin-to! "melpa" 'nix-ts-mode)

(pkg! 'nix-ts-mode)

(provide 'zy-nix)

;;; zy-nix.el ends here
