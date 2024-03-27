;;; zy-linum.el --- Line number display. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+linum' module of the configuration.

;; Line number is displayed via the `display-line-numbers-mode'. It can be
;; toggled by "<leader> y l".

;;; Code:

(require 'zylib)

;; Enable line numbers for any non-special mode.
(add-hook! 'after-change-major-mode-hook
  (unless (or (derived-mode-p 'special-mode)
              (derived-mode-p 'dired-mode))
    (display-line-numbers-mode 1)))

(after! 'display-line-numbers
  ;; Explicitly define a width to reduce the cost of on-the-fly computation. 4 is
  ;; a good default, as most text files do not exceed 10k lines.
  (setq display-line-numbers-width 4)

  ;; Show absolute line numbers for narrowed regions to make it easier to tell
  ;; the buffer is narrowed, and where you are, exactly.
  (setq display-line-numbers-widen t)

  ;; Set this value for a consistent line number width. It should be larger than
  ;; the number of lines a window normally has. Read the documentation.
  (setq display-line-numbers-width-start 60))

(provide 'zy-linum)

;;; zy-linum.el ends here
