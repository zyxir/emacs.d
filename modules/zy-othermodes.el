;;; zy-othermodes.el --- Other major modes. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+othermodes' module of the configuration.

;; I don't want to make a module for each file type, since supporting a lot of
;; them only requires installing a package, and does not require extensive
;; configuration. Therefore, for these "other modes", I put their package
;; installtion code here. However, if any mode requires further configuration
;; some day, I will extract their code into a dedicated module.

;; In addition, the package Valign is declared and managed here. This package
;; keeps tables in Org and Markdown files visually aligned.

;;; Code:

(require 'zylib)

(pkg! 'csv-mode)
(pkg! 'markdown-mode)
(pkg! 'nginx-mode)
(pkg! 'powershell)
(pkg! 'verilog-ts-mode)

(pkg! 'valign)

(add-hook! '(org-mode-hook markdown-mode-hook) (valign-mode 1))

(provide 'zy-othermodes)

;;; zy-othermodes.el ends here
