;;; zy-platform.el --- Platform-dependent settings. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+platform' module of the configuration.

;; Although Emacs is a cross-platform text editor, it requires much per-platform
;; configuration, especially when working with system utilities. This file is
;; the place for such settings.

;;; Code:

(require 'zylib)

(when (eq zy-platform 'wsl)
  (setq
   browse-url-browser-function
   (defun +platform-wsl-browse-url (url &rest _args)
     "Browse URL with the default browser on Windows host.
Unfortunately, _ARGS is not supported here."
     (call-process "explorer.exe" nil 0 nil url))))

(defun +platform--windows-open (file)
  "Open FILE in default external program.
This works for Microsoft Windows."
  (let* ((cmd (concat
               "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'"
               (shell-quote-argument (expand-file-name file)) "'")))
    (message cmd)
    (shell-command cmd)))

(defun +platform--linux-open (file)
  "Open FILE in default external program.
This works for Linux with the \"xdg-open\" command."
  (let* ((process-connection-type nil)
         (fullname (expand-file-name file))
         (cmd (format "xdg-open %s" fullname)))
    (message cmd)
    (call-process "xdg-open" nil 0 nil fullname)))

(defun +platform--wsl-open (file)
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
                          ((and (> len 0)
                                (eq (aref wslpath-output (- len 1)) ?\n))
                           (substring wslpath-output 0 (- len 1)))
                          (t wslpath-output)))
         (cmd (format "wslview %s" converted-path)))
    (message cmd)
    (call-process "wslview" nil 0 nil converted-path)))

(defvar +platform--open-fn
  (pcase zy-platform
    ('windows #'+platform--windows-open)
    ('linux #'+platform--linux-open)
    ('wsl #'+platform--wsl-open)
    (_ (lambda (&rest _) (user-error "Not supported on this platform"))))
  "Function used to open a file externally.")

(defun +platform/open-externally (&optional filename)
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
      (seq-map +platform--open-fn file-list))))

(after! '+leader
  (keybind! nil +leader-f-map
    "e" (cons "Open Externally" #'+platform/open-externally)))

(provide 'zy-platform)

;;; zy-platform.el ends here
