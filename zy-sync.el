;;; zy-sync.el --- Configuration synchronization. -*- lexical-binding: t -*-
;;; Commentary:

;; This file acts as a script to synchronize the configuration, which means to
;; install all packages required and re-compile all modified files, so that all
;; modifications take effect.
;;
;; The function `zy/sync', which run this script asynchronously from within
;; Emacs, is actually defined in init.el.

;;; Code:

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

(defconst zy-sync-force (getenv-internal "ZYEMACS_FORCE")
  "If we should force re-compilation of every file.
If this is nil, only compile those necessary.")

(defun zy-sync-setup ()
  "Do preparations for a synchronization."
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
    (package-quickstart-refresh)
    (load package-quickstart-file nil 'nomessage)))

(defun zy-sync--compile (file &optional native)
  "Byte-compile FILE.
If optional argument NATIVE is non-nil, also natively compile
FILE synchronously now. NATIVE has no effect if native
compilation is not available."
  (message "Compiling %s..." file)
  (byte-compile-file file)
  (when (and native
             (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (message "Natively compiling %s..." file)
    (native-compile file)))

(defun zy-sync-maybe-compile (file &optional native force)
  "Compile FILE if its byte code is outdated.
Always compile FILE if FORCE is non-nil. If optional argument
NATIVE is non-nil, also natively compile FILE synchronously.

Return t if FILE is actually compiled, or nil otherwise."
  (let* ((compiled (concat file "c"))
         (outdated (file-newer-than-file-p file compiled)))
    (when (or force outdated)
      (zy-sync--compile file native)
      t)))

(defun zy-sync-compile-all (&optional force native)
  "Re-compile every file.
What files are re-compiled is decided smartly. If FORCE is
non-nil, re-compile every file. If NATIVE is non-nil, also
do native compilation if available."
  (message "\n\nRe-compile everything%s"
           (if force " forcefully..." "..."))
  (let ((compiled-cnt 0))
    ;; Try to compile Zylib. If any component of Zylib is outdated and
    ;; re-compiled, everything else should be re-compiled as well, thus setting
    ;; `force' to t.
    (dolist (file zy-sync-zylib-files)
      (when (zy-sync-maybe-compile file native force)
        (setq force t)
        (setq compiled-cnt (+ compiled-cnt 1))))
    ;; Now compile each module.
    (dolist (file zy-sync-module-files)
      (when (zy-sync-maybe-compile file native force)
        (setq compiled-cnt (+ compiled-cnt 1))))
    (message "%s files re-compiled." compiled-cnt)))

;; This file only does its work if called non-interactively.
(when noninteractive
  (message "Synchronization begins.")
  (let* ((result
          (benchmark-run 1
            (zy-sync-setup)
            (zy-sync-ensure-packages)
            (zy-sync-compile-all zy-sync-force 'native)))
         (sec (car result)))
    (message "\n\nSynchronization finished in %s seconds." sec)))

(provide 'zy-sync)

;;; zy-sync.el ends here
