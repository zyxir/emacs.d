;;; zylib.el --- Always-available utilities. -*- lexical-binding: t -*-
;;; Commentary:

;; This is the library of the modular configuration. The modules of Zyxir's
;; Emacs configuration seldom depend on each other, but most of them depend on
;; this library, which provides necessary tools about package management, lazy
;; loading, among others.

;; This library is also divided into several components, each named as
;; zylib-NAME respectively.

;;; Code:

(require 'zylib-core)
(require 'zylib-pkg)
(require 'zylib-keybind)

(provide 'zylib)

;;; zylib.el ends here
