;;; init-personal.el --- Personal settings.  -*- lexical-binding: t -*-
;;; Commentary:

;; Most of these settings only appli when the user name equals "zyxir", which
;; means that the user is Eric Zyxir Zhuo Chen himself.

;;; Code:

(eval-and-compile (require 'init-basic))

(defconst zy/user-is-zyxir (equal user-login-name "zyxir")
  "If the user is Zyxir himself.")

(when (equal user-login-name "zyxir")
  (setq user-full-name "Eric Zhuo Chen"
        user-mail-address "ericzhuochen@outlook.com"))

(defun zy/-set-zybox-path (sym path)
  "Set SYM to PATH, and set other paths relative to Zybox.

This set up paths for many packages, including Org-agenda,
Org-journal, Org-roam, and many more."
  ;; Set the variable.
  (set sym path)
  ;; Set other things if the path is valid.
  (if (and (stringp path)
           (file-directory-p path)
           (equal (file-name-base path) "Zybox"))
      (progn
        ;; The Org directory.
        (defvar org-directory)
        (setq org-directory (expand-file-name "org" path))
        ;; My GTD files.
        (defvar zy-gtd-dir)
        (defvar zy-gtd-inbox-file)
        (defvar zy-gtd-gtd-file)
        (defvar zy-gtd-someday-file)
        (setq zy-gtd-dir org-directory
              zy-gtd-inbox-file
              (expand-file-name "inbox.org" zy-gtd-dir)
              zy-gtd-gtd-file
              (expand-file-name "gtd.org" zy-gtd-dir)
              zy-gtd-someday-file
              (expand-file-name "someday.org" zy-gtd-dir))
        ;; The Org-journal directory.
        (defvar org-journal-dir)
        (setq org-journal-dir
              (expand-file-name "org-journal" org-directory))
        ;; The Org-roam directory.
        (defvar org-roam-directory)
        (setq org-roam-directory
              (expand-file-name "org-roam" org-directory))
        ;; Ebib paths.
        (defvar ebib-preload-bib-files)
        (defvar ebib-notes-directory)
        (defvar ebib-file-search-dirs)
        (setq ebib-preload-bib-files
              `(,(expand-file-name "ebib/references.bib" path))
              ebib-notes-directory
              (expand-file-name "ebib/notes" path)
              ebib-file-search-dirs
              `(,(expand-file-name "ebib/files" path))))
    ;; When the path is invalid, report only when the user is Zyxir himself.
    (when zy/user-is-zyxir
      (lwarn 'zyemacs :warning "Zybox path is not valid."))))

(defcustom zy/zybox-path (or (zy/first-existing-path
                              "~/Zybox"
                              "/mnt/c/Users/zyxir/Zybox")
                             "/path/to/Zybox")
  "The directory containing Zyxir's personal files.

Once set to a valid path via the Customize interface, set all
relavant paths like `org-directory' as well."
  :type 'directory
  :set 'zy/-set-zybox-path
  :group 'zyemacs)

(provide 'init-personal)

;;; init-personal.el ends here
