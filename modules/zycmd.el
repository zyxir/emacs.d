;;; zycmd.el --- Useful commands.  -*- lexical-binding: t -*-

;; Author: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Maintainer: Eric Zhuo Chen
;; Version: 0.1
;; Package-Requires: ()
;; Homepage:
;; Keywords: commands

;;; Commentary:

;; This file provides autoloaded commands available to other modules of Zyxir's
;; Emacs config.

;;; Code:

(eval-and-compile (require 'init-elpa))

;;;; File-Related

;;;###autoload
(defun zy/delete-file-and-buffer (&optional trash)
  "Kill the current buffer and deletes the file it is visiting.

If TRASH is non-nil, move the file to trash."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun zy/-windows-open (file)
  "Open FILE in default external program.
This works for Microsoft Windows."
  (shell-command
   (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'"
           (shell-quote-argument (expand-file-name file)) "'")))

(defun zy/-linux-open (file)
  "Open FILE in default external program.
This works for Linux with the \"xdg-open\" command."
  (let ((process-connection-type nil))
    (call-process "xdg-open" nil 0 nil (expand-file-name file))))

(defun zy/-wsl-open (file)
  "Open FILE in Windows host with default program.
This works for WSL with wslu installed, which provides the
\"wslview\" command."
  (let* ((file-abs (expand-file-name file))
         (wslpath-output (with-output-to-string
                           (with-current-buffer standard-output
                             (call-process "wslpath" nil standard-output
                                           nil "-w" file-abs))))
         (len (length wslpath-output))
         (converted-path (cond
                          ((and (> len 0) (eq (aref wslpath-output (- len 1)) ?\n))
                           (substring wslpath-output 0 (- len 1)))
                          (t wslpath-output))))
    (message converted-path)
    (call-process "wslview" nil 0 nil converted-path)))

;; This is adapted from Xah Emacs.
;;;###autoload
(defun zy/open-externally (&optional filename)
  "Open FILENAME in default external program.
If FILENAME is omitted or nil, open visited file, or Dired marked
files."
  (interactive)
  (let* ((file-list (if filename
                        (list filename)
                      (if (and (derived-mode-p 'dired-mode)
                               (fboundp 'dired-get-marked-files))
                          (dired-get-marked-files)
                        (list (buffer-file-name)))))
         (do-it-p (if (<= (length file-list) 5)
                      t
                    (y-or-n-p "Open more than 5 files? "))))
    (when do-it-p
      (pcase zy-platform
       ('windows (mapc #'zy/-windows-open file-list))
       ('linux (mapc #'zy/-linux-open file-list))
       ('wsl (mapc #'zy/-wsl-open file-list))
       ('unsupported (message "Not supported on this OS."))))))

(provide 'zycmd)

;;; zycmd.el ends here
