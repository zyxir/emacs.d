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
        ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
        ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.ustc.edu.cn/elpa/stable-melpa/")))

;; Prefer packages from GNU ELPA and NonGNU ELPA, since packages in them are
;; considered part of GNU Emacs, have undergone stricter review upon
;; entering/updating, and are generally more stable.
(setq package-archive-priorities '(("gnu" . 4)
                                   ("nongnu" . 3)
                                   ("melpa" . 2)
                                   ("melpa-stable" . 1)))

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

(defun zy-get-package-version (package)
  "Get the version of PACKAGE as a string."
  (package-version-join
   (package-desc-version
    (package-get-descriptor package))))

(defun zy-require-package (package &optional info)
  "Make sure that PACKAGE is installed.
PACKAGE is a symbol representing a package.

If a 2nd optional argument INFO is omitted, install PACKAGE via
`package-install' when it is not installed.

If INFO is given as a string, and is a URL, install PACKAGE as a
version-controlled package from that URL.

If the package is successfully installed or already installed, it
is added to `zy-required-packages'."
  (unless (package-installed-p package)
    (let (;; Messages generated while installing a package generally do not
          ;; matter. We want to be notified only when an error occurs.
          (inhibit-message t))
      (cond
     ;; Case 1: No `info' provided. Install normally.
     ((not info) (zy--install-symbol-package package))
     ;; Case 2: `info' is a string. Install with VC.
     ((stringp info) (package-vc-install `(,package :url ,info)))
     ;; Other cases: invalid `info' provided.
     (t (error "Invalid package info: %s" info))))
    (message "Package `%s' with version %s installed."
             package (zy-get-package-version package)))
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

This macro is recognized at the synchronization stage to ensure a
package is installed."
  `(zy-require-package ,package ,info))

(defmacro pin-to! (archive package &rest packages)
  "Only install PACKAGES from ARCHIVE.
ARCHIVE is a package archive in `package-archives'. Each PACKAGE
is a quoted package symbol.

Some packages from ELPA are much outdated. For example, as of
March 2024, the ELPA version of Smartparens was updated in 2017,
while the Melpa version was updated within a month! Packages like
this have basically abandoned ELPA support, therefore it is sane
to only use their Melpa version.

\(fn PACKAGE [PACKAGE] ...)"
  (let* ((packages (cons package packages))
         (pin-forms (seq-map
                     (lambda (package)
                       `(add-to-list 'package-pinned-packages
                                     (cons ,package ,archive)))
                     packages)))
    `(progn
       ,@pin-forms
       (when noninteractive
         (message
          ,(format "Packages pinned to \"%s\": %s"
                   archive
                   (string-join
                    (seq-map (lambda (x)
                               (format "`%s'"
                                       (symbol-name (unquote! x))))
                             packages)
                    " ")))))))

(provide 'zylib-pkg)

;;; zylib-pkg.el ends here
