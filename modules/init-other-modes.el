;;; init-other-modes.el --- Other major modes.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file contains settings for several major-modes which require less
;; configuration.

;;; Code:

(eval-and-compile (require 'init-basic))

(pkg! 'csv-mode)
(pkg! 'markdown-mode)
(pkg! 'nginx-mode)
(pkg! 'pdf-tools)
(pkg! 'powershell)
(pkg! 'verilog-ts-mode)

(provide 'init-other-modes)

;;; init-other-modes.el ends here
