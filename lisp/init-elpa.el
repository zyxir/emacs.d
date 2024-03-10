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

;; On-demand installation of packages.
(defun require-package (package &optional min-version)
  "Make sure that PACKAGE is installed with MIN-VERSION.
If NO-REFRESH is nil, `package-refresh-contents' is called."
  (unless (package-installed-p package min-version)
    (unless (assoc package package-archive-contents)
      (message "Missing package: %s" package)
      (package-refresh-contents))
    (package-install package)))

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

;; Require some popular Emacs Lisp libraries.
(require-package 'dash)
(require-package 'f)

;; Require ESUP, the Emacs startup profiler.
(require-package 'esup)

(provide 'init-elpa)

;;; init-elpa.el ends here
