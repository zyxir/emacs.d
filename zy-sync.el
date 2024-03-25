;;; zy-sync.el --- Configuration synchronization. -*- lexical-binding: t -*-
;;; Commentary:

;; This file acts as an Emacs Lisp script to re-compile all files of Zyxir's
;; Emacs configuration.

;;; Code:

;; Let Emacs ignore the "--sync" and "--force-sync" arguments.
(add-to-list
 'command-line-functions
 (defun zy-sync-ignore-args-fn ()
   "Ignore arguments used to sync the config."
   (when (member argi '("--sync" "--force-sync"))
     (setq command-line-args-left
           (delete argi command-line-args-left))
     t)))

(defconst zy-sync-zylib-components
  (seq-map (lambda (x)
             (expand-file-name (format "lisp/%s.el" x) user-emacs-directory))
           '("zylib-core" "zylib-pkg" "zylib-key" "zylib"))
  "List of components of Zylib.")

(defconst zy-sync-modules
  (if (boundp 'zy-modules)
      ;; Only enabled modules.
      (seq-map (lambda (module)
                 (expand-file-name (format "modules/zy-%s.el"
                                           (substring (symbol-name module) 1))
                                   user-emacs-directory))
               zy-modules)
    ;; All available modules.
    (directory-files (expand-file-name "modules" user-emacs-directory)
                     'full "^zy-.*\\.el$"))
  "List of available modules of Zyxir's Emacs configuration.")

(defun zy-sync--read-sexp (buf)
  "Read one s-expression from buffer BUF.
If no s-expression can be read, return nil."
  (condition-case nil
      (read buf)
    (error nil)))

(defun zy-sync-ensure-pkgs-in (file)
  "Ensure that all packages required by FILE is installed."
  (require 'zylib-pkg)
  (message "Ensuring packages required by %s..." file)
  (let* ((buf (find-file-noselect file))
         (sexp nil))
    (with-current-buffer buf
      (goto-char (point-min)))
    (while (setq sexp (zy-sync--read-sexp buf))
      (when (eq (car sexp) 'pkg!)
        (eval sexp)
        (message "  `%s' ensured." (cadr (cadr sexp)))))
    (kill-buffer buf)))

(defun zy-sync-compile (file)
  "Byte-compile (and maybe natively compile) FILE."
  (let ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
        (modules-dir (expand-file-name "modules" user-emacs-directory)))
    (message "Compiling %s..." file)
    (call-process "emacs" nil "*ZyEmacs-Sync*" nil
                  "--batch" "--eval"
                  (format "
(progn
  (message \"Compiling %s...\")
  (add-to-list 'load-path \"%s\")
  (add-to-list 'load-path \"%s\")
  (package-initialize)
  (setq load-prefer-newer t)
  (byte-compile-file \"%s\")
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (native-compile-async \"%s\"))
  (message \"Compiling %s...done\"))
" file lisp-dir modules-dir file file file))
    (message "Compiling %s...done" file)))

(defun zy-sync-maybe-compile (file &optional force)
  "Compile FILE if its byte code is outdated.
Always compile FILE if FORCE is non-nil. Return t if FILE is
actually compiled, or nil otherwise."
  (let* ((compiled (concat file "c"))
         (outdated (file-newer-than-file-p file compiled)))
    (when (or force outdated)
      (zy-sync-compile file)
      t)))

(defun zy-sync-sync (&optional force)
  "Do a synchronization of Zyxir's Emacs configuration.
If FORCE is non-nil, force re-compile every file."
  (message "Synchronizing the configuration...")
  ;; First, install all packages.
  (dolist (file (append zy-sync-zylib-components zy-sync-modules))
    (zy-sync-ensure-pkgs-in file))
  ;; Second, refresh package cache to correctly autoload everything.
  (package-quickstart-refresh)
  (load package-quickstart-file 'noerror 'nomessage)
  ;; Third, try to compile Zylib. If any component of Zylib is compiled,
  ;; everything should be re-compiled, thus setting `force' to t.
  (dolist (component zy-sync-zylib-components)
    (when (zy-sync-maybe-compile component force)
      (setq force t)))
  ;; Finally, compile each module if: (a) `force' is non-nil, (b) it does not
  ;; have bytecode, or (c) its byte code is outdated.
  (dolist (module zy-sync-modules)
    (zy-sync-maybe-compile module force))
  (message "Synchronizing the configuration...done"))

;; Everything needed has been defined. Synchronize now!
(zy-sync-sync (bound-and-true-p zy-sync-force))

(provide 'zy-sync)

;;; zy-sync.el ends here
