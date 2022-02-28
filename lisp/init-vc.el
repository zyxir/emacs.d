;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Version control settings.

;;; Code:

;; Use magit as git interface.

(use-package magit
  :straight t
  :general
  (:keymaps 'ctl-x-map
	    "g" 'magit-status
	    "M-g" 'magit-dispatch))

;; End of config.

(provide 'init-vc)
