;;; zy-sync.el --- Configuration synchronization. -*- lexical-binding: t -*-
;;; Commentary:

;; This file acts as a script to synchronize the configuration, which means to
;; install all packages required and re-compile all modified files, so that all
;; modifications take effect.
;;
;; The function `zy/sync', which run this script asynchronously from within
;; Emacs, is actually defined in init.el.

;;; Code:

;; Always load the newer source code to prevent inconsistency.
(setq load-prefer-newer t)

(defconst zy-sync-zylib-files
  (seq-map (lambda (x)
             (expand-file-name (format "lisp/%s.el" x) user-emacs-directory))
           '("zylib-core" "zylib-pkg" "zylib-key" "zylib"))
  "List of components of Zylib, in their loading order.")

(defconst zy-sync-module-files
  (directory-files (expand-file-name "modules" user-emacs-directory)
                   'full "^zy-.*\\.el$")
  "List of all available module files.")

(defconst zy-sync-files (append zy-sync-zylib-files zy-sync-module-files)
  "List of all files that should be synchronized.")

(defconst zy-sync-buffer "*ZyEmacs-Sync*"
  "The buffer name for the synchronization process.")

(defconst zy-sync-level
  (let* (;; Change `maximum' according to the docstring.
         (maximum 2)
         (minimum 0)
         (env (getenv-internal "ZYEMACS_SYNC_LEVEL"))
         ;; `string-to-number' returns a number (integer or float) if the string
         ;; is actually a number, or 0 otherwise.
         (level (if (stringp env) (string-to-number env) 0))
         ;; A number may be an integer or a floating-point number. We only want
         ;; an integer.
         (level (if (integerp level) level 0))
         ;; Now we clamp the integer in the range.
         (level (if (> level maximum) maximum level))
         (level (if (< level minimum) minimum level)))
    level)
  "A number indicating the synchronization level.
The greater the number is, the more thourough the synchronization
will be, and the more time it will also take.

If `zy-sync-level' is 0, only install missing packages and
re-compile outdated files. However, if any file of Zylib is
updated, all module files will be re-compiled to prevent
incompatibility.

If `zy-sync-level' is 1, only install missing packages, but
re-compile every file (all byte code is cleaned before
re-compilation).

If `zy-sync-level' is 2, re-install every package (excluding
built-in ones and external ones), and re-compile every file.

The value is obtained via the environment variable
\"ZYEMACS_SYNC_LEVEL\". If it is not an integer, or if it is an
integer lesser than 0, it is treated as 0; if it is an integer
greater than the greatest possible value, it is treated as the
greatest possible value.")

(defun zy-sync-setup ()
  "Do preparations for a synchronization."
  ;; Remove the quickstart file(s).
  (let* ((file package-quickstart-file)
         (elc (concat file "c"))
         (files (list file elc)))
    (dolist (file files)
      (when (file-exists-p file)
        (delete-file file 'trash))))
  ;; Initialize packages if not yet.
  (unless (and (boundp 'package--initialized)
               package--initialized)
    (package-initialize))
  ;; Populate load path.
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
  ;; Load the Lisp code of zylib-core.el and zylib-pkg.el. Do not load their
  ;; byte code since it might be outdated.
  (dolist (relpath '("lisp/zylib-core.el"
                     "lisp/zylib-pkg.el"))
    (load (expand-file-name relpath user-emacs-directory)
          nil 'nomessage 'nosuffix)))

(defun zy-sync-clean-packages ()
  "Clean all installed packages and caches."
  (let ((default-directory user-emacs-directory))
    (message "\n\nCleaning installed packages and cache...")
    (dolist (dir `("eln-cache"
                   "elpa"
                   ,package-user-dir))
      (message "Trashing %s recursively..." (file-truename dir))
      (delete-directory dir 'recursive 'trash))))

(defun zy-sync--read-sexp (buf)
  "Read one s-expression from buffer BUF.
If no s-expression can be read, return nil."
  (condition-case nil
      (read buf)
    (error nil)))

(defvar zy-sync-pkg-fns '(pin-to! pkg!)
  "Package function to retrieve during synchronization.

During synchronization, these forms are executed according to
their orders in this list.")

(defun zy-sync--get-pkg-forms (file)
  "Get all package-related forms in FILE.

Currently it retrieves forms calling these functions/macros:

  `pkg!'    used to declare a required package
  `pin-to!' used to pin a package to an archive"
  (let* ((buf (find-file-noselect file))
         (sexp nil)
         (forms nil))
    (with-current-buffer buf
      (goto-char (point-min)))
    (while (setq sexp (zy-sync--read-sexp buf))
      (when (memq (car sexp) zy-sync-pkg-fns)
        (push sexp forms)))
    (kill-buffer buf)
    forms))

(defun zy-sync-ensure-packages ()
  "Ensure that all requested packages are installed."
  (let* ((pkg-forms (delete-dups
                     (mapcan #'zy-sync--get-pkg-forms zy-sync-files)))
         (sorted-forms (sort
                        pkg-forms
                        (lambda (form1 form2)
                          (let ((form1-smaller t)
                                (fns zy-sync-pkg-fns))
                            (while (and fns
                                        (not (eq (car fns) (car form1))))
                              (when (eq (car fns) (car form2))
                                (setq form1-smaller nil))
                              (setq fns (cdr fns)))
                            form1-smaller)))))
    (message "\n\nEnsuring packages...")
    ;; Ensure all packages.
    (dolist (form sorted-forms)
      (condition-case err
          (eval form)
        (error
         (message "`%s' encountered while evaluating `%s': %s"
                  (car err) form (cdr err)))))
    (message "%s packages ensured." (cl-list-length pkg-forms))
    ;; Reload the quickstart file.
    (message "Refreshing the package quickstart file...")
    (let ((inhibit-message t))
      (package-quickstart-refresh))
    (load package-quickstart-file 'noerror 'nomessage)))

(defun zy-sync-clean-elc ()
  "Clean all byte code."
  (let ((default-directory user-emacs-directory))
    (message "Cleaning byte code...")
    (dolist (dir `("lisp" "modules"))
      (dolist (elc (directory-files dir 'full "\\.elc$"))
        (delete-file elc nil)))))

(defun zy-sync--compile (file)
  "Byte-compile (and maybe natively compile) FILE."
  (message "Compiling %s..." file)
  (byte-compile-file file)
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (message "Natively compiling %s..." file)
    (native-compile file)))

(defun zy-sync-maybe-compile (file &optional force)
  "Compile FILE if its byte code is outdated.
Always compile FILE if FORCE is non-nil.

Return t if FILE is actually compiled, or nil otherwise."
  (let* ((compiled (concat file "c"))
         (outdated (file-newer-than-file-p file compiled)))
    (when (or force outdated)
      (zy-sync--compile file)
      t)))

(defun zy-sync-compile-all (&optional level)
  "Re-compile every file.
What files are re-compiled is decided smartly based on LEVEL. See
`zy-sync-level' for more information."
  (let ((force (>= level 1))
        (compiled-cnt 0))
    (message "\n\nRe-compile everything%s"
             (if force " forcefully..." "..."))
    ;; If we should forcefully re-compile everything, clean elc now.
    (when force (zy-sync-clean-elc))
    ;; Try to compile Zylib. If any component of Zylib is outdated and
    ;; re-compiled, everything else should be re-compiled as well, thus setting
    ;; `force' to t.
    (dolist (file zy-sync-zylib-files)
      (when (zy-sync-maybe-compile file force)
        (setq force t)
        (setq compiled-cnt (+ compiled-cnt 1))))
    ;; Now compile each module.
    (dolist (file zy-sync-module-files)
      (when (zy-sync-maybe-compile file force)
        (setq compiled-cnt (+ compiled-cnt 1))))
    (message "%s files re-compiled." compiled-cnt)))

;; This file only does its work if called non-interactively.
(when noninteractive
  (message "Synchronization begins with level %d." zy-sync-level)
  (let* ((result
          (benchmark-run 1
            (zy-sync-setup)
            (when (>= zy-sync-level 2)
              (zy-sync-clean-packages))
            (zy-sync-ensure-packages)
            (zy-sync-compile-all zy-sync-level)))
         (sec (car result)))
    (message "\n\nSynchronization finished in %s seconds." sec)))

(provide 'zy-sync)

;;; zy-sync.el ends here
