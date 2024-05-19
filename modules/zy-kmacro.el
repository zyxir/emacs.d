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
(advice-add
 #'kmacro-call-macro :around
 (defun +kmacro-disable-modes (oldfun &rest args)
   "Disable some minor modes while executing kmacros."
   (let ((orig-corfu-mode (if (bound-and-true-p corfu-mode) 1 -1)))
     (corfu-mode -1)
     (apply oldfun args)
     (corfu-mode orig-corfu-mode))))

(provide 'zy-kmacro)

;;; zy-kmacro.el ends here
