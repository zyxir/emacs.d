;;; init-common.el --- Configure common things -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-load)


;; Set flags

(setq auto-save-default nil
      disabled-command-function nil
      frame-title-format '("" "ZyEmacs" " [%b]")
      inhibit-compacting-font-caches t
      initial-scratch-message ""
      initial-major-mode 'fundamental-mode
      make-backup-files nil
      read-process-output-max (* 1024 1024)
      system-time-locale "C"
      use-dialog-box nil
      word-wrap-by-category t)

(setq-default fill-column 80)


;; Set everything to UTF-8.

(set-language-environment "UTF-8")


;; Inbuilt modes

(zy/delay-till-user-input
 (unless (display-graphic-p)
   (xterm-mouse-mode +1))
 (column-number-mode +1)
 (delete-selection-mode +1)
 (global-subword-mode +1)
 (blackout 'subword-mode)
 (recentf-mode +1))

(zy/delay-till after-find-file
  (global-auto-revert-mode +1)
  (save-place-mode +1)
  (require 'kinsoku))


(provide 'init-common)

;;; init-common.el ends here.
