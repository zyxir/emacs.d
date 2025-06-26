;;; init-modeline.el --- Modeline enhancements  -*- lexical-binding: t; -*-

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

;; This file provides a modeline based on Nano Modeline with extended
;; information display and enhanced mouse support.

;;; Code:

(require 'init-util)

(require-package 'nano-modeline)

;; Load Nano Modeline.
(require 'nano-modeline)
(declare-function warning-numeric-level "warning")
(declare-function flymake-show-buffer-diagnostics "flymake")
(declare-function flymake-show-project-diagnostics "flymake")
(declare-function flymake--diag-type "flymake")
(declare-function flymake--lookup-type-property "flymake")
(declare-function flymake--state-diags "flymake")
(defvar flymake--state)

;; Function to count flymake diagnostics.
(defun zy-count-diags ()
  "Count the number of diagnostics, grouped by level."
  (let ((warning-level (warning-numeric-level :warning))
        (note-level (warning-numeric-level :debug))
        (note 0) (warning 0) (error 0))
    (maphash (lambda (_b state)
               (cl-loop
                with diags = (flymake--state-diags state)
                for diag in diags do
                (let ((severity
		       (flymake--lookup-type-property
			(flymake--diag-type diag) 'severity
			(warning-numeric-level :error))))
                  (cond ((> severity warning-level) (cl-incf error))
                        ((> severity note-level) (cl-incf warning))
                        (t (cl-incf note))))))
             flymake--state)
    `((note . ,note) (warning . ,warning) (error . ,error))))

(defun zy-modeline-diag-face (type &optional face-prefix)
  "Return the appropriate face for TYPE diagnostic.

Use FACE-PREFIX per `nano-modeline-face'."
  (let* ((window (get-buffer-window (current-buffer)))
         (active (eq window nano-modeline--selected-window))
         (face (nano-modeline-face face-prefix))
         (color (face-attribute
                 (pcase type
                   ('note 'ansi-color-green)
                   ('warning 'flymake-warning-echo)
                   ('error 'flymake-error-echo))
                 :foreground nil 'inherit)))
    (if active
        `(:weight bold :inherit ((:foreground ,color) ,face))
      `(:inherit ,face))))

(defvar zy-modeline-diags-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
		#'flymake-show-buffer-diagnostics)
    (define-key map [header-line mouse-3]
		#'flymake-show-project-diagnostics)
    map))

;; Custom diagnostics info.
(defun zy-modeline-diags ()
  "Diagnostics information."
  (if (bound-and-true-p flymake-mode)
      (concat
       (let*
           ((content
             (let ((count-alist (zy-count-diags)))
	       (string-join
                (seq-keep
                 (lambda (type)
                   (let ((diag (alist-get type count-alist)))
                     (when (> diag 0)
                       (propertize
                        (format "%d" diag)
                        'face
                        (zy-modeline-diag-face type 'secondary)))))
                 '(error warning note))
                (propertize "/" 'face (nano-modeline-face 'secondary)))))
            (window (get-buffer-window (current-buffer)))
            (active (eq window nano-modeline--selected-window)))
         (if active
             (propertize
              content
              'help-echo "Flymake diagnostics
Left Click: Show Buffer Diagnostics
Right Click: Show Project Diagnostics"
              'mouse-face 'nano-salient
              'local-map zy-modeline-diags-map)
           content))
       (propertize "  " 'face (nano-modeline-face 'secondary)))
    ""))

;; Custom position indicator.
(defun zy-modeline-cursor-position ()
  "Cursor position in the buffer."
  (propertize (format-mode-line "%p%%  %l:%c ")
	      'face (nano-modeline-face 'secondary)))

;; Use a custom modeline composition.
(let ((left '((nano-modeline-buffer-status) " "
	      (nano-modeline-buffer-name) " "
	      (nano-modeline-git-info)))
      (right '((zy-modeline-diags)
	       (zy-modeline-cursor-position)
	       (nano-modeline-window-dedicated))))
 (nano-modeline-header left right 'default))

;; Disable the default modeline.
(setq-default mode-line-format nil)

(provide 'init-modeline)

;;; init-modeline.el ends here
