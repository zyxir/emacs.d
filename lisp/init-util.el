;;; init-util.el --- Utility functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Eric Zhuo Chen

;; Author: Eric Zhuo Chen <ericzhuochen@outlook.com>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines frequently used utility functions.

;;; Code:

;;;; Package Management

(require 'package)

;; Only use packages from GNU ELPA, NonGNU ELPA, and Melpa.
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Prefer packages from GNU ELPA and NonGNU ELPA for better stability,
;; reliability, and security.
(setq package-archive-priorities '(("gnu" . 4)
				   ("nongnu" . 3)
				   ("melpa-stable" . 2)
				   ("melpa" . 1)))

;; Enable the quickstart feature. This feature concatenates all
;; autoload statements emitted by installed packages into a single
;; file, the `package-quickstart-file', thus speeding up startup
;; greatly if there are a lot of packages. However, if the quickstart
;; file is not updated timely for some reason and this causes trouble,
;; run `package-quickstart-refresh' manually to fix it.
(setq package-quickstart t)

;; Load the quickstart file at compile time to diminish warnings.
(eval-when-compile (load package-quickstart-file))

;; This function programmatically ensures that a package is installed.
(defun require-package (package)
  "Ensure PACKAGE is installed."
  (unless (package-installed-p package)
    ;; If the package is not found, refresh contents for once.
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package))
  ;; Return whether the package has been installed.
  (package-installed-p package))

;;;; Lazy Loading

(defun zy--require-when-compile (flat-fts)
  "Require every feature in FLAT-FTS during byte compilation.
FLAT-FTS is a list of feature symbols."
  (when (bound-and-true-p byte-compile-current-file)
    (seq-map #'require flat-fts)))

(defun zy--gen-after-load (norm-fts body)
  "Generate statements to load BODY after NORM-FTS.
NORM-FTS is a normalized nested feature list returned by
`zy--process-features'.

The list can contain keywords `:any' and `:all', where no
keywords implied `:all'.

ALl features will be required at compile time to silence compiler
warnings."
  (cond
   ((and norm-fts (symbolp norm-fts))
    `((eval-after-load ',norm-fts ',(macroexp-progn body))))
   ((and (consp norm-fts) (memq (car norm-fts) '(:or :any)))
    (cl-mapcan #'(lambda (x) (zy--gen-after-load x body)) (cdr norm-fts)))
   ((and (consp norm-fts) (memq (car norm-fts) '(:and :all)))
    (cl-dolist (next (cdr norm-fts))
      (setq body (zy--gen-after-load next body)))
    body)
   ((listp norm-fts)
    (zy--gen-after-load (cons :all norm-fts) body))))

(defun zy--process-features (features)
  "Convert FEATURES for macro expansion.
FEATURES can be:

- A quoted symbol.

- A quoted nested list of symbols.

The list can contain keywords `:any' and `:all', where no
keywords implied `:all'.

This function returns (NORM-FTS FLAT-FTS), where:

- NORM-FTS is the normalized FERATURES, where there is no quote and
  every symbol is converted.

- FLAT-FTS is the flattened list of features.  All keywords are
  excluded."
  ;; Remove the quote.
  (setq features (cadr features))
  ;; Normalize and flatten elements.
  (cl-labels ((normalize (symbol) symbol)
              (normalize-recursively (features)
                (if (listp features)
                    (mapcar #'normalize-recursively features)
                  (normalize features)))
              (flatten-recursively (norm-fts)
                (if (listp norm-fts)
                    (mapcan #'flatten-recursively norm-fts)
                  (unless (keywordp norm-fts)
                    (list norm-fts)))))
    (let* ((norm-fts (normalize-recursively features))
           (flat-fts (flatten-recursively norm-fts)))
      (list norm-fts flat-fts))))

(defmacro zy-eval-after-load (features* &rest body)
  "Evaluate BODY after FEATURES* are available.

FEATURES* can be a symbol or a nested list of symbols.  It should be
quoted.  If a symbol starts with the plus sign like `+leader' does, it
is considered a module of the configuration, and will be processed
accordingly.  It can contain keywords like `:all' and `:any'."
  (declare (indent 1) (debug (form def-body)))
  (let* ((result (zy--process-features features*))
         (features* (car result))
         (flist (cadr result)))
    ;; While byte-compiling the file, require all features so that all its
    ;; symbols are in scope, like `use-package' does.
    (zy--require-when-compile flist)
    ;; Generate the `eval-after-load' statement.
    (macroexp-progn
     (zy--gen-after-load features* body))))

;;;; Filesystem

(defun zy-choose-dir (&rest dirs)
  "Return the first existing directory in DIRS."
  (cl-flet ((dir-exists-p (dir)
	      (and (file-directory-p dir)
		   dir)))
    (cl-some #'dir-exists-p dirs)))

;;;; Leader Key

;; Define a leader map to group custom keybindings.
(defvar-keymap zy-leader-map)

;;;; Personal Cloud

;; My personal cloud is the basis of multiple mechanisms.
(defcustom zy-zyhub-dir ""
  "Directory of ZyHub, my personal cloud."
  :type 'directory
  :group 'zyemacs
  :set (defun zy-set-zyhub-dir (symbol value)
	 "Set SYMBOL to VALUE."
	 (set symbol value)
	 ;; Set Org-roam directory.
	 (setq-default org-roam-directory
		       (expand-file-name "Roam" value))))

;; If the user is me, setup personal cloud automatically.
(when (and (equal user-login-name "zyxir")
	   (equal zy-zyhub-dir ""))
  (when-let
      ((chosen-dir
	(zy-choose-dir
	 "~/Library/CloudStorage/坚果云-zyxirchen@outlook.com/ZyHub")))
    (customize-save-variable 'zy-zyhub-dir chosen-dir)))

(provide 'init-util)

;;; init-util.el ends here
