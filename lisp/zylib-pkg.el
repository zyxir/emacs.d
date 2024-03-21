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

;; Initialize package.el if not initialized yet. It might have been initialized
;; while byte-compiling some code or expanding some macros.
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(defvar zy-required-packages '()
  "Packages explicitly required by `pkg!'.")

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

(defun zy-require-package (package &optional repo)
  "Make sure that PACKAGE is installed.
PACKAGE is a symbol representing a package.

If REPO is given and is a URL, install PACKAGE as a
version-controlled package from that URL. Otherwise install
PACKAGE from one of the package repositories.

If the package is installed, it is added to
`zy-required-packages'."
  (unless (package-installed-p package)
    (if repo
        (package-vc-install `(,package :url ,repo))
      (zy--install-symbol-package package)))
  ;; Add the package symbol to `package-selected-packages' to prevent it from
  ;; being auto-removed. Normally this shouldn't be done manually, but we do
  ;; this just in case the list is unexpectedly modified.
  (add-to-list 'package-selected-packages package)
  ;; Track explicitly required packages.
  (add-to-list 'zy-required-packages package)
  ;; Return the package symbol.
  package)

(defmacro pkg! (package &optional repo)
  "Make sure that PACKAGE is installed.
PACKAGE is a package name, either quoted or unquoted.

If REPO is given and is a URL, install PACKAGE as a
version-controlled package from that URL. Otherwise install
PACKAGE from one of the package repositories.

If the package is installed, it is added to
`zy-required-packages'.

This is a macro wrapped around `zy-require-package'. The
difference is that this macro tries to install the package
required during byte compilation, so that the code depending on
the required package could be compiled correctly."
  `(eval-and-compile (zy-require-package ',(unquote! package) ,repo)))

(provide 'zylib-pkg)

;;; zylib-pkg.el ends here
