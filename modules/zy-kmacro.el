;;; zy-kmacro.el --- Keyboard macros. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+kmacro' module of the configuration.

;; Keyboard macro is very powerful in Emacs. In fact, the name "Emacs"
;; originally was an acronym for Editor MACroS according to Richard M. Stallman.
;; Keyboard macros work out of the box, with <F3> and <F4> as their keybindings
;; (or the "q" key in Evil). This module provides several additional tweaks to
;; them.

;;; Code:

(require 'zylib)

;; Disable unnecessary minor modes before performing a keyboard macro, and
;; re-enable them afterwards.

(defvar +kmacro--corfu-mode nil
  "The original state of `corfu-mode'.")

(advice-add
 #'kmacro-call-macro :before
 (defun +kmacro-disable-modes-before-exec-a (&rest _)
   "Disable some minor modes and record their states."
   (when (fboundp 'corfu-mode)
     (setq +kmacro--corfu-mode (and (boundp 'corfu-mode) corfu-mode))
     (corfu-mode -1))))

(advice-add
 #'kmacro-call-macro :after
 (defun +kmacro-restore-modes-after-exec-a (&rest _)
   "Restore some minor modes based on recorded states."
   (when (fboundp 'corfu-mode)
     (corfu-mode (if +kmacro--corfu-mode 1 -1)))))

(provide 'zy-kmacro)

;;; zy-kmacro.el ends here
