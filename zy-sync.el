;;; zy-sync.el --- Configuration synchronization. -*- lexical-binding: t -*-
;;; Commentary:

;; This file acts as an Emacs Lisp script to re-compile all files of Zyxir's
;; Emacs configuration.

;;; Code:

(require 'comint)

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

(defconst zy-sync-files (append zy-sync-zylib-components zy-sync-modules)
  "List of files to synchronize.")

(defvar zy-sync--proc nil
  "The Emacs subprocess used for synchronization.")

(defvar zy-sync--proc-buffer "*ZyEmacs-Sync*"
  "The buffer name for the Emacs subprocess.")

(defvar zy-sync--proc-init-form
  `(progn
     (package-initialize)
     (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
     (add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
     (require 'zylib)
     (message "The Emacs subprocess is ready.")
     (while t (eval (read-minibuffer ""))))
  "Initialization form for the Emacs subprocess.")

(defun zy-sync--proc-create ()
  "Create and return the the Emacs subprocess."
  (start-process "emacs" zy-sync--proc-buffer "emacs"
                 "--batch" "--eval"
                 (format "%S" zy-sync--proc-init-form)))

(defun zy-sync-eval (form)
  "Evaluate FORM in the Emacs subprocess.
FORM is an s-expression."
  (unless zy-sync--proc
    (setq zy-sync--proc (zy-sync--proc-create)))
  (let ((print-escape-newlines t)
        (print-escape-control-characters t))
    (process-send-string zy-sync--proc (format "%S\n" form))))

(defun zy-sync-msg (format-string &rest args)
  "Display a message in the Emacs subprocess.
The message is formatted with FORMAT-STRING with ARGS."
  (zy-sync-eval `(message ,(apply #'format format-string args))))

(defun zy-sync-stop-proc ()
  "Properly stop the Emacs subprocess."
  ;; Ask the process to quit.
  (zy-sync-eval `(kill-emacs))
  ;; Wait until it is actually stopped.
  (while (process-live-p zy-sync--proc)
    (ignore)))

(defun zy-sync--read-sexp (buf)
  "Read one s-expression from buffer BUF.
If no s-expression can be read, return nil."
  (condition-case nil
      (read buf)
    (error nil)))

(defun zy-sync--get-pkg-forms (file)
  "Get all `pkg!' forms in FILE."
  (let* ((buf (find-file-noselect file))
         (sexp nil)
         (forms nil))
    (with-current-buffer buf
      (goto-char (point-min)))
    (while (setq sexp (zy-sync--read-sexp buf))
      (when (eq (car sexp) 'pkg!)
        (push sexp forms)))
    (kill-buffer buf)
    forms))

(defun zy-sync-ensure-packages ()
  "Ensure that all requested packages are installed."
  (let ((pkg-forms (delete-dups
                    (mapcan #'zy-sync--get-pkg-forms zy-sync-files))))
    (zy-sync-msg "\n\nEnsuring packages...")
    ;; Ensure all packages.
    (dolist (form pkg-forms)
      (zy-sync-eval form)
      (when init-file-debug
        (zy-sync-msg "  `%s' ensured." (cadr (cadr form)))))
    (zy-sync-msg "%s packages ensured." (cl-list-length pkg-forms))
    ;; Reload the quickstart file.
    (zy-sync-msg "Refreshing the package quickstart file...")
    (zy-sync-eval `(with-no-warnings
                     (package-quickstart-refresh)))
    (zy-sync-eval `(with-no-warnings
                     (load package-quickstart-file nil 'nomessage)))))

(defun zy-sync--compile (file)
  "Byte-compile (and maybe natively compile) FILE."
  (zy-sync-eval `(byte-compile-file ,file))
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (zy-sync-msg "Compiling %s..." file)
    (zy-sync-eval `(native-compile-async ,file))))

(defun zy-sync-maybe-compile (file &optional force)
  "Compile FILE if its byte code is outdated.
Always compile FILE if FORCE is non-nil. Return t if FILE is
actually compiled, or nil otherwise."
  (let* ((compiled (concat file "c"))
         (outdated (file-newer-than-file-p file compiled)))
    (when (or force outdated)
      (zy-sync--compile file)
      t)))

(defun zy-sync-compile-all (&optional force)
  "Re-compile every file.
What files are re-compiled is decided smartly. If FORCE is
non-nil, re-compile every file."
  (zy-sync-msg "\n\nRe-compile everything%s"
               (if force " forcefully..." "..."))
  (let ((compiled-cnt 0))
    ;; Try to compile Zylib. If any component of Zylib is outdated and
    ;; re-compiled, everything else should be re-compiled as well, thus setting
    ;; `force' to t.
    (dolist (component zy-sync-zylib-components)
      (when (zy-sync-maybe-compile component force)
        (setq force t)
        (setq compiled-cnt (+ compiled-cnt 1))))
    ;; Now compile each module.
    (dolist (module zy-sync-modules)
      (when (zy-sync-maybe-compile module force)
        (setq compiled-cnt (+ compiled-cnt 1))))
    (zy-sync-msg "%s files re-compiled." compiled-cnt)))

(defun zy-sync-sync (&optional force)
  "Do a synchronization of Zyxir's Emacs configuration.
If FORCE is non-nil, force re-compile every file."
  (message "Synchronizing the configuration...")
  (message "See buffer %s for details" zy-sync--proc-buffer)
  (let* ((result
          (benchmark-run 1
            ;; Make sure all packages are installed.
            (zy-sync-ensure-packages)
            ;; Re-compile everything smartly.
            (zy-sync-compile-all force)
            ;; Kill the process.
            (zy-sync-stop-proc)))
         (secs (car result)))
  (message "Synchronizing completed in %s seconds." secs)))

;; Everything needed has been defined. Synchronize now!
(zy-sync-sync (bound-and-true-p zy-sync-force))

(provide 'zy-sync)

;;; zy-sync.el ends here
