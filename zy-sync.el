;;; zy-sync.el --- Configuration synchronization. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:

;; This file acts as an Emacs Lisp script to re-compile all files of Zyxir's
;; Emacs configuration.

;;; Code:

(defun compile (file)
  "Compile FILE in every way possible."
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
  (when (and (fboundp 'native-comp-available-p)
             (native-comp-available-p))
    (native-compile \"%s\"))
  (message \"Compiling %s...done\"))
" file lisp-dir modules-dir file file file))
    (message "Compiling %s...done" file)))

;; Compile every file of Zylib.
(dolist (file '("lisp/zylib-core.el"
                "lisp/zylib-pkg.el"
                "lisp/zylib-key.el"
                "lisp/zylib.el"))
  (compile (expand-file-name file user-emacs-directory)))

;; Compile every module in "modules".
(dolist (module (directory-files
                 (expand-file-name "modules" user-emacs-directory)
                 'full "^zy-.+\\.el$"))
  (compile module))

(provide 'zy-sync)

;;; zy-sync.el ends here
