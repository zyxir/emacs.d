;;; zy-personal.el --- Personal settings -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+personal' module of the configuration.

;; Most of these settings only apply when the user name equals "zyxir", which
;; means that the user is Eric Zyxir Zhuo Chen himself.

;;; Code:

(require 'zylib)

(defconst +personal-enable (equal user-login-name "zyxir")
  "The `+personal' module only works for Zyxir himself.")

(when +personal-enable
  (setq user-full-name "Eric Zhuo Chen"
        user-mail-address "ericzhuochen@outlook.com"))

(defun +personal--set-zybox-path (sym path)
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
        ;; Org agenda files.
        (defvar +gtd-file)
        (setq +gtd-file (expand-file-name "gtd.org" org-directory))
        (defvar +gtd-notebook-file)
        (setq +gtd-notebook-file (expand-file-name "notebook.org" org-directory))
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
              `(,(expand-file-name "ebib/files" path)))
        ;; Citar paths (same as the Ebib ones).
        (defvar citar-bibliography)
        (setq citar-bibliography
              `(,(expand-file-name "ebib/references.bib" path))))
    ;; Otherwise, warn about it when necessary.
    (when +personal-enable
      (lwarn 'zyemacs :warning "Zybox path is not valid."))))

(defcustom +personal-zybox-path (or (some-path!
                                     "~/Zybox"
                                     "/mnt/c/Users/zyxir/Zybox")
                                    "/path/to/Zybox")
  "The directory containing Zyxir's personal files.

Once set to a valid path via the Customize interface, set all
relavant paths like `org-directory' as well."
  :type 'directory
  :set '+personal--set-zybox-path
  :group 'zyemacs)

(provide 'zy-personal)

;;; zy-personal.el ends here
