;;; zy-corfu.el --- Text completion with Corfu. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+corfu' module of the configuration.

;; Corfu is the simplest completion UI for Emacs out there, and it supports
;; anything which uses `completing-in-region'. Apart from Corfu itself, this
;; module also incorporates several auxiliary modes for Corfu.
;;
;; Corfu does not have its own completion backends. It uses
;; `completion-at-point-functions' (CAPFs) instead. The package Cape provides
;; several useful CAPFs, and it is also required in the `+quickins' module for
;; quick text insertion.

;;; Code:

(require 'zylib)

(pkg! 'corfu)
(pkg! 'cape)

;; Enable Corfu globally. For prog modes Corfu is obviously a necessity. For
;; text modes, however, Corfu provides a useful UI for Dabbrev expansion.
(global-corfu-mode 1)

(after! 'corfu
  ;; Enable auto completion.
  (setq corfu-auto t)

  ;; No delay for auto completion.
  (setq corfu-auto-delay 0)

  ;; Do not intefere with cursor movement keys.
  (keybind! nil corfu-map
    "RET" nil
    "<up>" nil
    "<down>" nil
    [remap previous-line] nil
    [remap next-line] nil)

  ;; Remember completion history.
  (corfu-history-mode 1)

  ;; Show candidate help message in the echo area after a short delay.
  (setq-default corfu-echo-delay '(1.0 . 0))
  (corfu-echo-mode 1))

;; Enable Corfu in the minibuffer.

(add-hook! 'minibuffer-setup-hook
  (defun +corfu-enable-in-minibuffer-h ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1))))

(provide 'zy-corfu)

;;; zy-corfu.el ends here
