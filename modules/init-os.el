;;; init-os.el --- OS-specific settings.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

;; Windows Subsystem for Linux.
(when (eq zy/os 'wsl)
  ;; Browse URL with explorer.
  (setq browse-url-browser-function
        (defun zy/wsl-browse-url (url &rest _)
          "Open URL with \"wslview\"."
          (call-process "wslview" nil 0 nil url))))

(provide 'init-os)

;;; init-os.el ends here
