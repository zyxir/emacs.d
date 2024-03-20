;;; init-modeline.el --- Mode line customization.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-and-compile (require 'init-basic))

(require-package 'doom-modeline)

(require 'doom-modeline)

;; Customize the mode line with Doom-modeline.
(setq
 ;; Always adjust the line height based on font size.
 doom-modeline-height 1
 ;; Show word count.
 doom-modeline-enable-word-count t
 ;; Only display miscellaneous information on the active mode line.
 doom-modeline-display-misc-in-all-mode-lines nil
 ;; Do not use icons.
 doom-modeline-icon nil
 ;; Do not show environment version.
 doom-modeline-env-version nil)

;; Activate the mode line.
(doom-modeline-mode 1)

;; HACK Add some padding at the right side to prevent cutoff. This tweak is not
;; clean, expect problems to happen if `doom-modeline' is updated.
(defun zy/-doom-modeline-set-right-padding (sym val)
  "Set the right padding of the Doom mode line.
SYM is intended to be `zy/doom-modeline-right-padding', and VAL
is a string representing the padding."
  (set-default-toplevel-value sym val)
  (doom-modeline-def-modeline 'main
    '( eldoc bar workspace-name window-number modals matches follow
       buffer-info remote-host buffer-position word-count parrot
       selection-info)
    `( compilation objed-state misc-info persp-name battery grip
       irc mu4e gnus github debug repl lsp minor-modes input-method
       indent-info buffer-encoding major-mode process vcs check time
       ,val)))

(defcustom zy/doom-modeline-right-padding "    "
  "The padding added at the right of the Doom mode line.
Adjsut it according to the actual display of the device."
  :type 'string
  :set #'zy/-doom-modeline-set-right-padding
  :group 'zyemacs)

;; Enable column number display.
(column-number-mode 1)

(provide 'init-modeline)

;;; init-modeline.el ends here
