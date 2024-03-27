;;; zylib-pkg.el --- Package management. -*- lexical-binding: t -*-
;;; Commentary:

;; This file defines the package management framework of the configuration. It
;; provides a simple yet feature-rich macro, `pkg!', to require packages on
;; demand.

;;; Code:

(require 'zylib-core)

;; Install packages into separate directories for each Emacs version to prevent
;; byte code incompatibility.
(setq package-user-dir
      (expand-file-name (format "elpa-%s" emacs-version)
                        user-emacs-directory))

;; Setup USTC mirror as the package source.
(setq package-archives
      '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
        ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

;; Prefer packages from GNU ELPA and NonGNU ELPA, since packages in them are
;; considered part of GNU Emacs, have undergone stricter review upon
;; entering/updating, and are generally more stable.
(setq package-archive-priorities '(("gnu" . 3)
                                   ("nongnu" . 2)
                                   ("melpa" . 1)))

;; Natively compile packages at installation. If native compilation support is
;; absent, this option does not have any effect.
(setq package-native-compile t)

;; Enable the quickstart feature. This feature concatenates all autoload
;; statements emitted by installed packages into a single file, the
;; `package-quickstart-file', thus speeding up startup greatly if there are a
;; lot of packages. However, if the quickstart file is not updated timely for
;; some reason and this causes trouble, run `package-quickstart-refresh'
;; manually to fix it.
(setq package-quickstart t)

(defvar zy-required-packages '()
  "Packages explicitly required by `pkg!'.")

;; Now that the list of packages is declared in the configuration instead of
;; controlled by manual `package-install' calls, we should make sure that
;; `package-selected-packages' cooresponds to `zy-required-packages' after
;; startup, so that utility functions like `package-autoremove' work properlly.
(add-hook! 'window-setup-hook
  (setq package-selected-packages zy-required-packages))

(defun zy--install-symbol-package (package &optional no-refresh)
  "Install PACKAGE, a symbol representing a package.

If NO-REFRESH is nil, and a file-error occurs during
installation, re-download the list of available packages and try
to install the package again."
  (if no-refresh
      (package-install package)
    (condition-case nil
        (package-install package)
      (file-error
       (package-refresh-contents)
       (zy--install-symbol-package package 'no-refresh)))))

(defun zy-require-package (package &optional info)
  "Make sure that PACKAGE is installed.
PACKAGE is a symbol representing a package.

If a 2nd optional argument INFO is omitted, install PACKAGE via
`package-install' when it is not installed.

If INFO is given as a string, and is the name of one of the
`package-archives', only install the package from that archive.

If INFO is given as a string, and is a URL, install PACKAGE as a
version-controlled package from that URL.

If the package is successfully installed or already installed, it
is added to `zy-required-packages'."
  (unless (memq package package-activated-list)
    (cond
     ;; Case 1: No `info' provided. Install normally.
     ((not info) (zy--install-symbol-package package))
     ;; Case 2: `info' is a package archive. Pin the package to that archive
     ;; before installing normally.
     ((assoc info package-archives)
      (let ((package-pinned-packages `((,package . ,info))))
        (zy--install-symbol-package package)))
     ;; Case 3: `info' is a string. Install with VC.
     ((stringp info) (package-vc-install `(,package :url ,info)))
     ;; Other cases: invalid `info' provided.
     (t (error "Invalid package info: %s" info))))
  ;; Track explicitly required packages.
  (add-to-list 'zy-required-packages package)
  ;; Return the package symbol.
  package)

(defmacro pkg! (package &optional info)
  "Make sure that PACKAGE is installed.
PACKAGE is a package name, either quoted or unquoted.

If a 2nd optional argument INFO is omitted, install PACKAGE via
`package-install' when it is not installed.

If INFO is given as a string, and is the name of one of the
`package-archives', only install the package from that archive.

If INFO is given as a string, and is a URL, install PACKAGE as a
version-controlled package from that URL.

If the package is successfully installed or already installed, it
is added to `zy-required-packages'.

This is a macro wrapped around `zy-require-package'. The
difference is that this macro tries to install the package
required during byte compilation, so that the code depending on
the required package could be compiled correctly."
  `(zy-require-package ,package ,info))

(provide 'zylib-pkg)

;;; zylib-pkg.el ends here
