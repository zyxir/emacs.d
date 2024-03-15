;;; init-elpa.el --- Install packages.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl-lib))

;; Determine the running environment.
(defvar zy/wsl-p (file-exists-p "/etc/wsl.conf")
  "If Emacs is running on WSL.")
(defvar zy/os
  (cond ((member system-type '(ms-dos windows-nt cygwin))
	 'windows)
	((eq system-type 'gnu/linux)
	 (if zy/wsl-p 'wsl 'linux))
	(t 'unsupported))
  "The operating system Emacs is running on.
Possible values:
  `windows'     Microsoft Windows
  `wsl'         Windows subsystem for Linux
  `linux'       a Linux distribution
  `unsupported' an unsupported system")

;; Install packages into separate directories for each Emacs version to prevent
;; byte code incompatibility.
(setq package-user-dir
      (expand-file-name (format "elpa-%s" emacs-version)
                        user-emacs-directory))

;; Setup USTC mirror as the package source.
(setq package-archives
      '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.ustc.edu.cn/elpa/stable-melpa/")
        ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

;;;; On-Demand Installation of Packages

;; This is a handy system based on Purcell's config.

(defvar required-packages '()
  "Packages required by `require-package'.")

(defun zy/-require-symbol-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.

If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to correctly install PACKAGE."
  (when (stringp min-version)
    (setq min-version (version-to-list min-version)))
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car (sort known
                              (lambda (a b)
                                (version-list-<= (package-desc-version b)
                                                 (package-desc-version a)))))))
        (if (and best (version-list-<= min-version (package-desc-version best)))
            (if no-refresh
                (package-install package)
              ;; Even if there is a best pacakge, there might be `file-error'
              ;; thrown while trying to fetch its dependencies. Try to refresh
              ;; the package contents to solve this.
              (condition-case nil
                  (package-install package)
                (file-error
                 (package-refresh-contents)
                 (zy/-require-symbol-package package min-version t))))
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (zy/-require-symbol-package package min-version t)))
        (package-installed-p package min-version))))

(defun zy/-require-local-package (path)
  "Install package from local file/directory PATH."
  (defvar zy/module-dir)
  (let* ((relative-p (null (file-name-absolute-p path)))
         (expanded-by-module-dir
          (when relative-p (expand-file-name path zy/module-dir)))
         (path-choices (if relative-p
                           `(,expanded-by-module-dir)
                         `(,path)))
         (final-path (cl-some (lambda (path) (when (file-exists-p path) path))
                              path-choices)))
    (if final-path
        (package-install-file final-path)
      (error "Cannot find an existing path from %s" path-choices))))

(defun require-package (package &optional min-version)
  "Make sure that PACKAGE is installed with MIN-VERSION.

If PACKAGE is a symbol, it is installed with `package-install'
from one of the `package-archives'.

If PACKAGE has the form (NAME . SPEC), where SPEC is a plist
describing a package from a VC source as described in Info
node `(emacs)Fetching Package Sources', it is installed with
`package-vc-install'.

If PACKAGE has the form (NAME . PATH) or (NAME PATH), where PATH
is a filesystem path, it is installed with
`package-install-file'. If PATH is relative, it is interpreted
based on `zy/lisp-dir' or `zy/site-lisp-dir'.

Anyway, if the package is installed, it is added to
`required-packages'."
  (let* ((name (if (consp package) (car package) package))
         (spec-or-path (cdr-safe package))
         (path (if (stringp spec-or-path)
                   spec-or-path
                 (when (and (consp spec-or-path)
                            (stringp (car-safe spec-or-path)))
                   (car spec-or-path))))
         (spec (when (plistp spec-or-path) spec-or-path)))
    (unless (package-installed-p name min-version)
      (cond
       ;; A package symbol.
       ((null spec-or-path) (zy/-require-symbol-package name min-version))
       ;; A VC package.
       (spec (package-vc-install package))
       ;; A local package.
       (path (zy/-require-local-package path))
       ;; Unknown cases.
       (t (error "%s does not describe a valid package" package))))
    ;; Add the package symbol to `package-selected-packages' to prevent it from
    ;; being auto-removed. Normally this shouldn't be done manually, but we do
    ;; this just in case the list is unexpectedly modified.
    (add-to-list 'package-selected-packages name)
    ;; Track explicitly required packages.
    (add-to-list 'required-packages package)
    package))

;; Start package.el.
(setq package-native-compile t)
(package-initialize)

;; Refresh package archives at first install.
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; Require some popular Emacs Lisp libraries.
(require-package 'dash)
(require-package 'f)

;; Require ESUP, the Emacs startup profiler.
(require-package 'esup)

;; Enable GCMH to reduce GC lags.
(require-package 'gcmh)
(gcmh-mode 1)

(provide 'init-elpa)

;;; init-elpa.el ends here
