;;; zyutils.el --- autoloaded utilities -*- lexical-binding: t -*-

;; Copyright (C) 2022-2022 Eric Zhuo Chen

;; Author: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Maintainer: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Created: 2022-10-29


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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file extends init.el by providing additional functionalities for many of
;; its sections.  However, this file should not be loaded at startup, which
;; creates additional load time.  Autoload each function needed explicitly at
;; every section instead.

;; Magic comment (;;;###autoload) is tagged to those functions that should be
;; autoloaded, but just as a hint.  Autoloads are actually explicitly written in
;; init.el.

;; This file is sectioned just like init.el, providing better correspondence.

;;; Code:

;;;; Base settings

;;;;; Scratch buffer

;;;###autoload
(defun zy/scratch ()
  "Switch to the default scratch buffer.
The buffer is automatically converted to text mode."
  (interactive)
  (let ((buf (get-buffer-create "*scratch*")))
    (switch-to-buffer buf)
    (text-mode)))

;;;###autoload
(defun zy/scratch-elisp ()
  "Switch to or create the Lisp interaction scratch buffer.
The buffer is automatically converted to Lisp interaction mode."
  (interactive)
  (let ((buf (get-buffer-create "*scratch-lisp*")))
    (switch-to-buffer buf)
    (lisp-interaction-mode)))

;;;###autoload
(defun zy/scratch-org ()
  "Switch to or create the Org scratch buffer.
The buffer is automatically converted to Org mode."
  (interactive)
  (let ((buf (get-buffer-create "*scratch-org*")))
    (switch-to-buffer buf)
    (org-mode)))

;;;; Text-editing

;;;;; Cursor movement

;; This is adapted from Crux.
(defvar zy-line-start-regexp-alist
  '((term-mode . "^[^#$%>\n]*[#$%>] ")
    (eshell-mode . "^[^$\n]*$ ")
    (org-mode . "^\\(\*\\|[[:space:]]*\\)* ")
    (default . "^[[:space:]]*"))
  "Alist of major modes and line starts.
The key is a major mode.  The value is a regular expression
matching the characters to be skipped over.  If no major mode is
found, use the regexp specified by the default key.

Used by functions like `zy/move-beginning-of-line' to skip over
whitespace, prompts, and markup at the beginning of the line.")

;; This is adapted from Crux.
(defun zy/move-to-line-start ()
  "Move to the beginning, skipping mode specific line start regexp."
  (interactive)
  (beginning-of-line nil)
  (let ((line-start-regexp (cdr (seq-find
                                 (lambda (e) (derived-mode-p (car e)))
                                 zy-line-start-regexp-alist
                                 (assoc 'default
                                        zy-line-start-regexp-alist)))))
    (search-forward-regexp line-start-regexp (line-end-position) t)))

;; This is adapted from Crux.
;;;###autoload
(defun zy/move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

When moving from position that has no ‘field’ property, this
command doesn’t enter text which has non-nil ‘field’ property.
In particular, when invoked in the minibuffer, the command will
stop short of entering the text of the minibuffer prompt.  See
‘inhibit-field-text-motion’ for how to inhibit this.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
\(But if the buffer doesn’t end in a newline, it stops at the
beginning of the last line.)If ARG is not nil or 1, move forward
ARG - 1 lines first.  If point reaches the beginning or end of
the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first.
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  ;; Move the point.
  (let ((orig-point (point)))
    (zy/move-to-line-start)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;;; Line filling

;;;###autoload
(defun zy/unfill-paragraph ()
  "Do the inverse of `fill-paragraph'."
  (interactive)
  (dlet ((fill-column most-positive-fixnum))
    (call-interactively 'fill-paragraph)))

;;;; Workbench

;;;;; File operations

;; This is copied from Crux.
;;;###autoload
(defun zy/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; This is copied from Crux.
;;;###autoload
(defun zy/rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-file-name "New name: " (file-name-directory filename)))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(declare-function dired-get-marked-files 'dired)

;; This is adapted from Xah Emacs.
;;;###autoload
(defun zy/open-externally (&optional filename)
  "Open FILENAME in default external program.
If FILENAME is omitted or nil, open visited file, or Dired marked
files."
  (interactive)
  (let* ((file-list (if filename
                        (list filename)
                      (if (string-equal major-mode "dired-mode")
                          (dired-get-marked-files)
                        (list (buffer-file-name)))))
         (do-it-p (if (<= (length file-list) 5)
                      t
                    (y-or-n-p "Open more than 5 files? "))))
    (when do-it-p
      (cond
       ((eq system-type 'windows-nt)
        (mapc
         (lambda (fpath)
           (shell-command
            (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'"
                    (shell-quote-argument (expand-file-name fpath )) "'")))
         file-list))
       ((eq system-type 'darwin)
        (mapc
         (lambda (fpath)
           (shell-command
            (concat "open " (shell-quote-argument fpath))))
         file-list))
       ((eq system-type 'gnu/linux)
        (mapc
         (lambda (fpath)
           (if (fboundp 'zy/open-file-with-explorer)
               (zy/open-file-with-explorer fpath)
             (let ((process-connection-type nil))
               (start-process "" nil "xdg-open" fpath))))
         file-list))))))

;;;; File type specific settings

;;;;; Org

;;;;;; Org export to LaTeX

;;;###autoload
(defun zy/update-zylatex-file ()
  "Update zylatex.sty from GitHub or existing project."
  (interactive)
  (require 'ox-latex)
  (defvar zy-zyprojects-dir)
  (defvar zy-zylatex-file)
  (let (ego-path
        zylatex-file)
    (if ;; Try to find the zylatex.sty from local repository.
        (and
         ;; The Zyprojects directory exists.
         zy-zyprojects-dir
         ;; The "ego" repository exists in Zyprojects.
         (setq ego-path (locate-file "ego" (list zy-zyprojects-dir) nil
                                     (lambda (&rest _) 'dir-ok)))
         ;; The "zylatex.sty" file exists in "ego".
         (setq zylatex-file (locate-file "std/std-latex/zylatex.sty"
                                         (list ego-path))))
        ;; If local zylatex.sty is found, copy it here.
        (progn
          (copy-file zylatex-file zy-zylatex-file
                     'ok-if-already-exists 'keep-time
                     'preserve-uid-gid 'preserve-permissions)
          (message "\"zylatex.sty\" copied from project \"ego\""))
      ;; If cannot locate zylatex.sty locally, download it from GitHub.
      (url-copy-file
       "https://raw.githubusercontent.com/zyxir/std-latex/main/zylatex.sty"
       zy-zylatex-file 'ok-if-already-exists)
      (message "\"zylatex.sty\" downloaded."))))

;;;;; Python

;;;###autoload
(defun zy/run-python-script ()
  "Run the current Python script with `python-shell-interpreter'."
  (interactive)
  (if (derived-mode-p 'python-base-mode)
      (let* ((interpreter (if (boundp 'python-shell-interpreter)
                              python-shell-interpreter
                            "python"))
             (file (or (buffer-file-name)
                       (make-temp-file (buffer-name) nil nil (buffer-string)))))
        (compile (concat interpreter " " file)))))

;;;; The end

(provide 'zyutils)
;;; zyutils.el ends here

;; Local Variables:
;; fill-column: 90
;; End:
