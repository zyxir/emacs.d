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
;;
;; When a package is only available on Melpa, prefer Melpa-stable, since tagged
;; versions are usually more stable than the master branch. For instance, as of
;; 2024-03-27, the master branch of Dashboard breaks, but the tagged version
;; works.
;;
;; However, sometimes a package in Melpa-stable may be very outdated. For
;; example, as of 2024-03-28, Smartparens on Melpa-stable was updated in 2017.
;; In this case it is preferred to pin it to a specific package archive with the
;; `pin-to!' command.
(setq package-archive-priorities '(("gnu" . 4)
                                   ("nongnu" . 3)
                                   ("melpa-stable" . 2)
                                   ("melpa" . 1)))

;; When using a mirror, signature checking sometimes causes problems. Let's
;; disable signature checking altogether.
(setq package-check-signature nil)

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

;; Now we should do `package-initialize' if it is not initialized yet.
(unless (bound-and-true-p package--initialized)
  (package-initialize))

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

(defun zy--install-site-lisp-package (subdir)
  "Install SUBDIR in the site-lisp directory."
  (package-install-file
   (expand-file-name
    subdir
    (expand-file-name "site-lisp" user-emacs-directory))))

(defun zy-get-package-version (package)
  "Get the version of PACKAGE as a string."
  (or (package-version-join
       (package-desc-version
        (package-get-descriptor package)))
      "unknown"))

(defun zy-require-package (package &rest spec)
  "Make sure that PACKAGE is installed.
PACKAGE is a symbol representing a package.

If no optional argument SPEC is provided, install the package
PACKAGE via `package-install' when it is not installed. It
respects packages pinned to a specific archive via `pin-to!', and
respects `package-archive-priority'.

Otherwise, if SPEC is a property list describing a package
specification specifying a package from source, which will be
installed via `package-vc-install', as described in the Info
node `(emacs)Fetching Package Sources'.

SPEC can also contain a single string, which is the name of a
sub-directory in the \"site-lisp\" directory in
`user-emacs-directory'. This way we can install a package pinned
to a specific commit as a git submodule.

If the package is successfully installed or already installed, it
is added to `zy-required-packages', which will eventually replace
the content of `package-selected-packages'."
  ;; Make sure that packages are initialized.
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  ;; Install the package unless it is already installed yet.
  (unless (package-installed-p package)
    (let* ((type (cond
                  ;; Case 1: No `spec' provided. Install normally.
                  ((not spec) 'normal)
                  ;; Case 2: `spec' is a plist. Install with VC.
                  ((plistp spec) 'vc)
                  ;; Case 3: `spec' is a string. Install local file.
                  ((and (not (cdr-safe spec)) (stringp (car-safe spec)))
                   'local)
                  ;; Other cases: invalid `spec' provided.
                  (t (error "Invalid package specification: %s" spec))))
           (type-indicator (pcase type
                             ('normal "")
                             ('vc " (via VC)")
                             ('local " (via git submodule)"))))
      (let (;; Messages generated while installing a package generally do not
            ;; matter. We want to be notified only when an error occurs.
            (inhibit-message t))
        (pcase type
          ('normal (zy--install-symbol-package package))
          ('vc (package-vc-install (cons package spec)))
          ('local (zy--install-site-lisp-package (car spec)))))
      (message "Package `%s' with version %s installed%s."
               package (zy-get-package-version package) type-indicator)))
  ;; Track explicitly required packages.
  (add-to-list 'zy-required-packages package)
  ;; Return the package symbol.
  package)

(defmacro pkg! (package &rest spec)
  "Make sure that PACKAGE is installed.
PACKAGE is a symbol representing a package.

If no optional argument SPEC is provided, install the package
PACKAGE via `package-install' when it is not installed. It
respects packages pinned to a specific archive via `pin-to!', and
respects `package-archive-priority'.

Otherwise, SPEC should be a property list describing a package
specification specifying a package from source, which will be
installed via `package-vc-install', as described in the Info
node `(emacs)Fetching Package Sources'.

If the package is successfully installed or already installed, it
is added to `zy-required-packages', which will eventually replace
the content of `package-selected-packages'.

If the package is successfully installed or already installed, it
is added to `zy-required-packages'.

This macro is recognized at the synchronization stage to ensure a
package is installed."
  `(zy-require-package ,package ,@spec))

(defvar package-pinned-packages)

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
