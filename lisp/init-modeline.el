;;; init-modeline.el --- Customized modeline  -*- lexical-binding: t; -*-

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

;; This file provides a modeline which mimics the elegance and
;; minimalism of nano-modeline, roughly organized as:

;; [ status | name (primary) <--> secondary ]

;; The modeline is actually hidden in this configuration.  Information
;; is displayed in the header line instead.

;; Faces of this modeline are based on those from nano-theme.

;;; Code:

(require 'cl-lib)
(require 'init-util)

(require-package 'nano-theme)

;; Load Nano Modeline.
(require 'nano-modeline)

;;;; Define faces based on `nano-theme'.

(defface zy-modeline-active
  `((t (:inherit header-line)))
  "Face when line is active.")

(defface zy-modeline-inactive
  `((t (:inherit (zy-modeline-active))))
  "Face when line is inactive.")

(defface zy-modeline-status-base
  `((t ( :foreground ,(face-background 'default)
         :background ,(face-foreground 'shadow nil 'inherit)
         :inherit bold)))
  "Face for line status.")

(defface zy-modeline-status-modified
  `((t ( :inherit (nano-popout-i zy-modeline-status-base))))
  "Face for line status when buffer is modified.")

(defface zy-modeline-name
  `((t ( :inherit (bold zy-modeline-active))))
  "Face for buffer name.")

(defface zy-modeline-error
  `((t ( :inherit (ansi-color-red bold zy-modeline-active))))
  "Face for error count.")

(defface zy-modeline-warning
  `((t ( :inherit (flymake-warning-echo bold zy-modeline-active))))
  "Face for warning count.")

(defface zy-modeline-note
  `((t ( :inherit (ansi-color-green bold zy-modeline-active))))
  "Face for note count.")

(defface zy-modeline-secondary
  `((t ( :inherit (nano-faded))))
  "Face for secondary information.")

(defface zy-modeline-mouse
  `((t ( :inherit (nano-salient))))
  "Face for mouse-over.")

(defvar zy-modeline--selected-window nil
  "The currently selected window.")

(defun zy-modeline--update-selected-window ()
  "Update selected window."
  (setq zy-modeline--selected-window (selected-window)))

(add-hook 'post-command-hook #'zy-modeline--update-selected-window)

(defun zy-modeline-active-p ()
  "Return non-nil if modeline should be active."
  (eq (get-buffer-window (current-buffer))
      zy-modeline--selected-window))

(defun zy-modeline-face (&optional face-prefix)
  "Return the modeline face corresponding to current active state.

If FACE-PREFIX is nil, `zy-modeline-active' or `zy-modeline-inactive' is
returned.  Otherwise a face with the corresponding prefix is returned.
`zy-modeline-inactive' is used as the default inactive face if no
inactive face with that prefix is found."
  (let* ((active (zy-modeline-active-p))
         (prefix (if face-prefix
                     (concat "zy-modeline-" (symbol-name face-prefix))
                   "zy-modeline")))
    (if active
        (if (facep (intern (concat prefix "-active")))
            (intern (concat prefix "-active"))
          (if (facep (intern prefix))
              (intern prefix)
            (warn "No face named `%s'" prefix)
            'zy-modeline-active))
      (if (facep (intern (concat prefix "-inactive")))
          (intern (concat prefix "-inactive"))
        'zy-modeline-inactive))))

;;;; Define other utility functions

;; Helper function to build a segment.
(cl-defun zy-modeline-seg
    (parts &key
           (face-prefix nil)
           (help-echo nil)
           (mouse-face nil)
           (local-map nil))
  "Build a segment using PARTS corresponding to ACTIVE.

Argument PARTS is a list of strings or a single string representing
segment contents.  PARTS can contain format strings per
`mode-line-format'.

Argument ACTIVE should be non-nil if the current window is active, or
nil otherwise.  It is provided to avoid redundant computations.

Keyword argument FACE-PREFIX specifies the face prefix applied to the
segment per `zy-modeline-face'.  If any part in PARTS is already
propertized with a face, it is not further propertized.

Keyword arguments HELP-ECHO, MOUSE-FACE, and LOCAL-MAP are effective
only when ACTIVE is non-nil.

Keyword argument HELP-ECHO specifies the tooltip string displayed when
mouse hovers over the segment.

Keyword argument MOUSE-FACE specifies the face applied when mouse hovers
over the segment.

Keyword argument LOCAL-MAP specifies the keymap to make active when
mouse hovers over the segment."
  (let* ((active (zy-modeline-active-p))
         (face (zy-modeline-face face-prefix))
         (parts (if (listp parts) parts (list parts)))
         (parts (seq-map (lambda (part)
                           (if (get-text-property 0 'face part)
                               part
                             (propertize part 'face face)))
                         parts))
         (content (string-join parts)))
    (if active
        (let ((args (list content)))
          (when help-echo
            (setq args (append args (list 'help-echo help-echo))))
          (when mouse-face
            (setq args (append args (list 'mouse-face mouse-face))))
          (when local-map
            (setq args (append args (list 'local-map local-map))))
          (apply #'propertize args))
      content)))

;;;; The status segment

(defun zy-modeline--status-seg ()
  "Segment displaying buffer status."
  (cl-flet (;; Wrap the content in horizontal and vertical paddings.
            (wrap (content)
              (list (propertize " " 'display '(raise 0.2))
                    content
                    (propertize " " 'display '(raise -0.25)))))
    (cond
     (buffer-read-only
      (zy-modeline-seg (wrap "RO")
                       :face-prefix 'status-base
                       :help-echo "The buffer is read-only."))
     ((buffer-modified-p)
      (zy-modeline-seg (wrap "**")
                       :face-prefix 'status-modified
                       :help-echo "The buffer is modified."))
     (t
      (zy-modeline-seg (wrap "RW")
                       :face-prefix 'status-base
                       :help-echo "The buffer is read-write.")))))

;;;; The buffer name segment

(defun zy-modeline--buffer-full-name ()
  "Get the full name of the buffer."
  (or (buffer-file-name (current-buffer))
      (buffer-name)))

(defun zy-modeline-copy-buffer-full-name ()
  "Copy the full name of the buffer."
  (interactive)
  (let ((name (zy-modeline--buffer-full-name)))
    (kill-new name)
    (message name)))

(defvar zy-modeline--name-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                #'zy-modeline-copy-buffer-full-name)
    map)
  "Keymap for the name segment.")

(defun zy-modeline--name-echo
    (_window _object _pos)
  "Echo message of the name segment."
  (format "%s
Left Click: copy buffer/file name" (zy-modeline--buffer-full-name)))

(defun zy-modeline--name-seg ()
  "Segment displaying buffer name."
  (zy-modeline-seg
   (cond ((buffer-narrowed-p) (format "%s [narrow]" (buffer-name)))
         (t (buffer-name)))
   :face-prefix 'name
   :help-echo #'zy-modeline--name-echo
   :mouse-face 'nano-salient
   :local-map zy-modeline--name-map))

;;;; The git segment

(defun zy-modeline--git-seg ()
  "Segment displaying git info."
  (when vc-mode
    (when-let* ((file (buffer-file-name))
                (branch (substring-no-properties vc-mode 5))
                (state (vc-state file)))
      (zy-modeline-seg (format "(%s%s, %s)" " " branch state)))))

;;;; The diagnostics segment

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
               (dolist (diag (flymake--state-diags state))
                 (let ((severity
		        (flymake--lookup-type-property
			 (flymake--diag-type diag) 'severity
			 (warning-numeric-level :error))))
                   (cond ((> severity warning-level) (cl-incf error))
                         ((> severity note-level) (cl-incf warning))
                         (t (cl-incf note))))))
             flymake--state)
    `((note . ,note) (warning . ,warning) (error . ,error))))

(defvar zy-modeline--diags-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
		#'flymake-show-buffer-diagnostics)
    (define-key map [header-line mouse-3]
		#'flymake-show-project-diagnostics)
    map)
  "Keymap for modeline diagnostics.")

(defun zy-modeline--diags-seg ()
  "Segment displaying buffer diagnostics."
  (when (bound-and-true-p flymake-mode)
    (zy-modeline-seg
     (let* ((diags (zy-count-diags))
            (diag-count (seq-reduce #'+ (seq-map #'cdr diags) 0)))
       (if (> diag-count 0)
           (string-join
            (seq-keep (lambda (type)
                        (let* ((diag (alist-get type diags))
                               (diag-str (format "%d" diag)))
                          (when (> diag 0)
                            (zy-modeline-seg diag-str :face-prefix type))))
                      '( error warning note))
            "/")
         (zy-modeline-seg
          (if (trusted-content-p)
              ""
            "")
          :face-prefix 'secondary)))
     :help-echo (defun zy-modeline--diags-echo
                    (_window _object _pos)
                  (format
                   "%s
Left Click: show buffer diagnostics
Right Click: show project diagnostics"
                   (if (trusted-content-p)
                       "Buffer diagnostics"
                     "Buffer not trusted.  No diagnostics available")))
     :mouse-face 'nano-salient
     :local-map zy-modeline--diags-map)))

;;;; The position segment

(defun zy-modeline--percentage ()
  "Display buffer percentage with 3 characters."
  (let ((orig (format-mode-line "%o")))
    (cond
     ((string= orig "Bottom") "Bot")
     ((string-match "\\`[0-9]+%\\'" orig)
      (replace-regexp-in-string "%" "%%" orig))
     (t orig))))

(defun zy-modeline--pos-echo (window _object _pos)
  "Echo message of the position segment.
WINDOW is the current window."
  (with-current-buffer (window-buffer window)
    (let ((line (line-number-at-pos))
          (column (current-column))
          (pos (format-mode-line "%o")))
      (format "Line: %d, Column: %d, Buffer Position: %s"
              line column pos))))

(defun zy-modeline--pos-seg ()
  "Segment displaying cursor position."
  (zy-modeline-seg
   (list "%l:%c " (zy-modeline--percentage))
   :face-prefix 'secondary
   :help-echo #'zy-modeline--pos-echo))

;;;; Setup the modeline using segments defined above

;; The function that builds the complete modeline format.
(defun zy-modeline-build ()
  "Build the complete modeline format string."
  (let* (;; Define the left and right part.
         (left (string-join
                (seq-keep
                 #'identity
                 (list (zy-modeline--status-seg)
                       (zy-modeline--name-seg)
                       (zy-modeline--git-seg)))
                " "))
         (right (string-join
                 (seq-keep
                  #'identity
                  (list (zy-modeline--diags-seg)
                        (zy-modeline--pos-seg)))
                 "  "))
         ;; Define widths.
         (width (window-width))
         (outside fringes-outside-margins)
         (left-fringe (if outside -1.0 0.0))
         (left-margin (if outside 0.0 1.0))
         (right-fringe (if outside -1.0 0.0))
         (right-margin (if outside -1.0 0.0))
         (left-max-size (- width (length right) 2))
         ;; Adjust the left part.
         (left (if (> (length left) left-max-size)
                   (concat (truncate-string-to-width left left-max-size)
                           (zy-modeline-seg "…" :mouse-face nil))
                 left)))
    (concat
     (propertize " "
                 'display `(space :align-to (+ left-margin
                                               (,left-fringe . left-fringe)
                                               (,left-margin . left-margin))))
     left
     (propertize " "
                 'face (zy-modeline-face)
                 'display `(space :align-to (- right-margin
                                               (,right-fringe . right-fringe)
                                               (,right-margin . right-margin)
                                               2
                                               ,(length right))))
     right)))

;; Place our custom modeline in the header.
(setq-default header-line-format
              '(:eval (zy-modeline-build)))
(face-remap-set-base 'header-line 'zy-modeline-active)

;; Disable the bottom modeline.
(setq-default mode-line-format nil)

;; (nano-modeline-text-mode 'default)

(provide 'init-modeline)

;;; init-modeline.el ends here
