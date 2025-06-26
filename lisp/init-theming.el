;;; init-theming.el --- Look and feel  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Eric Zhuo Chen

;; Author: Eric Zhuo Chen <ericzhuochen@outlook.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file dictates the look and feel of Emacs.

;;; Code:

(require 'cl-lib)
(require 'init-util)

(require-package 'nano-theme)

;;;; Themes

;; The Nano Theme offers a simple and elegant aesthetic.  Since Emacs
;; is primarily used for text work in this configuration, enable Nano
;; Theme if no other custom theme has been specified.
(unless custom-enabled-themes
  (load-theme 'nano-light 'no-confirm))

;; Do not extend region selection to the edge of the window.
(set-face-attribute 'region nil :extend nil)

;;;; Layout

;; Use a layout similar to NANO Emacs.
(setq default-frame-alist
      '((internal-border-width . 24)
	(left-fringe . 1)
	(right-fringe . 1)))

;; Enable window dividers.
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;;;; Fonts

;; Tweak fonts until all following text displays:
;; [Chinese] 業精於勤，荒於嬉；行成於思，毀於隨。
;; [English] Sphinx of black quartz, judge my vow.
;; [Russian] Эх, чужак! Общий съём цен шляп (юфть)—вдрызг!

;;  Setting fonts in Emacs is notoriously complicated---this function
;; distills dozens of hours of refinement into a single interface.
(defun zy-set-charset-font (face charset font)
  "Set FONT as the font for CHARSET and FACE.

CHARSET specifies the character set or the list of character sets to set
font for.

FONT is the font to be set.  It can either be a `font-spec' object or a
font name string."
  (let* (;; The fontset that we are going to manipulate
	 (fontset (face-attribute face :fontset))
	 ;; If the fontset is not specified yet
	 (unspecified (equal fontset 'unspecified)))
    ;; If the fontset is not specified, create a new one with a
    ;; programmatically generated name
    (when unspecified
      (setq fontset
            (new-fontset
             (format "-*-*-*-*-*--*-*-*-*-*-*-fontset-zy%s"
                     (cl-gensym))
             nil)))
    ;; Set font for the fontset
    (if (listp charset)
	(mapc (lambda (c)
		(set-fontset-font fontset c font nil 'prepend))
	      charset)
      (set-fontset-font fontset charset font nil))
    ;; Assign the fontset to the face if necessary
    (when unspecified
      (set-face-attribute face nil :fontset fontset))))

;; This platform-specific list defines fonts for several faces and
;; character sets.
(defvar zy-font-list
  (cond
   ((eq system-type 'darwin)
    '("MesloLGS NF" "PingFang TC" "Times New Roman" "Songti SC"))
   ((eq system-type 'windows-nt)
    '("Cascadia Code" "Microsoft JhengHei" "Times New Roman" "SimSun"))
   (t nil))
  "Platform-specific font list.

1st and 2nd fonts: latin and CJK font for the `default' and
`fixed-pitch' faces.

3rd and 4th fonts: latin and CJK font for the `variable-pitch' face.")

;; Customize this variable for machine-specific font height.
(defcustom zy-font-height 140
  "Font height for all character sets."
  :type 'float
  :group 'zyemacs
  :set (defun zy-set-font-height (symbol value)
	 "Set SYMBOL to VALUE."
	 (set symbol value)
	 (set-face-attribute 'default nil :height value)))

;; Provide commands and menu entries to adjust font height.
(defun zy-inc-font-size ()
  "Increment default font size."
  (interactive)
  (customize-save-variable 'zy-font-height
			  (+ zy-font-height 10)))
(defun zy-dec-font-size ()
  "Decrement default font size."
  (interactive)
  (customize-save-variable 'zy-font-height
			  (- zy-font-height 10)))
(define-key-after menu-bar-custom-menu [separator]
  '(menu-item "--"))
(define-key-after menu-bar-custom-menu [zy-inc-font-size]
  '(menu-item "[ℤ] Increment Default Font Size"
	      zy-inc-font-size
	      :help "Increment font size by 1 pixel and save it"))
(define-key-after menu-bar-custom-menu [zy-dec-font-size]
  '(menu-item "[ℤ] Decrement Default Font Size"
	      zy-dec-font-size
	      :help "Decrement font size by 1 pixel and save it"))

;; Set font when the display is graphic and the font list is defined.
(when (and (display-graphic-p) zy-font-list)
  (set-face-attribute 'default nil :height zy-font-height)
  (let ((cjk-charsets '(han cjk-misc bopomofo kana hangul)))
    (dolist (face '(default fixed-pitch))
      (set-face-attribute face nil
                          :family (nth 0 zy-font-list))
      (zy-set-charset-font face cjk-charsets
                           (nth 1 zy-font-list)))
    (set-face-attribute 'variable-pitch nil
                        :family (nth 2 zy-font-list)
                        :height (lambda (h) (+ h 30)))
    (zy-set-charset-font 'variable-pitch cjk-charsets
                         (nth 3 zy-font-list))))

;; Render `org-code' in monospace font, but in smaller size.
(zy-eval-after-load 'org
  (set-face-attribute 'org-code nil
                      :family (nth 0 zy-font-list)
                      :height 0.9))

(provide 'init-theming)

;;; init-theming.el ends here
