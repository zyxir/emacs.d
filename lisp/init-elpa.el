;;; init-elpa.el --- Install packages.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(defvar required-packages '()
  "Packages required by `require-package'.")

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
       ((null spec-or-path)
        (let* ((known (cdr (assoc package package-archive-contents)))
               (best (car (sort known (lambda (a b)
                                        (version-list-<=
                                         (package-desc-version b)
                                         (package-desc-version a)))))))
          (if (and best (version-list-<= min-version
                                         (package-desc-version best)))
              (package-install best)
            (error "No version of %s >= %S is available" package min-version))))
       ;; A VC package.
       (spec
        (package-vc-install package))
       ;; A local package.
       (path
        (let* ((relative-p (null (file-name-absolute-p path)))
               (lisp-dir-path
                (when relative-p
                  (expand-file-name path zy/lisp-dir)))
               (site-lisp-dir-path
                (when relative-p
                  (expand-file-name path zy/site-lisp-dir)))
               (path-choices (if relative-p
                                 `(,lisp-dir-path ,site-lisp-dir-path)
                               `(,path)))
               (final-path (cl-some (lambda (path) (when (file-exists-p path) path))
				    path-choices)))
          (if final-path
              (package-install-file final-path)
            (error "Cannot find an existing path from %s." path-choices))))
       ;; Unknown cases.
       (t
        (error "%s does not describe a valid package." package))))
    (add-to-list 'required-packages package)
    package))

;; On-demand installation of packages via VC.
(defun require-vc-package (package &optional rev backend)
  "Make sure that PACKAGE is installed with REV.
PACKAGE is of the form (NAME . SPEC), where NAME is a symbol
designating the package and SPEC is a property list describing a
package specification as in the Info node `(emacs)Fetching
Package Sources'.

REV and BACKEND work similarly as in `pacakge-vc-install'."
  (require 'package-vc)
  (unless (package-installed-p (car package))
    (package-vc-install package rev backend)))


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

(provide 'init-elpa)

;;; init-elpa.el ends here
