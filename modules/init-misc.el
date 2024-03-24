;;; init-misc.el --- Miscellaneous setup.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file includes miscellaneous settings that is hard to categorize.

;;; Code:

(eval-and-compile (require 'init-basic))

(defer!
  ;; Always match parenthesis.
  (electric-pair-mode 1)

  ;; Always show matching parenthesis.
  (show-paren-mode 1))

(provide 'init-misc)

;;; init-misc.el ends here
