;;; init.el --- Boostrap config modules.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Check Emacs version.
(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Emacs %s or higher is required to run Zyxir's config" minver)))

;; Make these directories available to Emacs. They contain most files of this
;; configuration. Although all Lisp libraries and modules are loaded with their
;; full path in this file, populating `load-path' ensures correct dependency
;; management in byte compilation.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Enable/disable modules here.
(defvar zy-modules
  '(;; Use saner default values.
    +defaults
    ;; Setup Evil, leader-prefixed keys, and key hints.
    +evil
    +leader
    +keyhint
    ;; Enhance the workbench.
    +theme
    +font
    +modeline
    +orderless
    +minibuffer
    +persist
    +dashboard
    +linum
    ;; +indentbar
    +indentguide
    +tab
    +icon
    +treemacs
    ;; Platform or environment-specific.
    +terminal
    +platform
    +personal
    ;; File/directory/project management.
    +project
    +direnv
    +file
    +dired
    ;; Applications or features.
    +git
    +gtd
    +journal
    +lingual
    ;; Text-editing and coding.
    +embark
    +corfu
    +dabbrev
    +paragraph
    +outline
    +quickins
    +search
    +yasnippet
    +syncheck
    +eldoc
    +undo
    +pair
    +editorconfig
    +eglot
    +treesit
    +prose
    +kmacro
    ;; Programming Languages or file formats.
    +elisp
    +nix
    +org
    +pdf
    +python
    +scala
    +tex
    +othermodes
    ;; Setup GCMH last to prevent GC during startup.
    +gcmh)
  "Enabled modules of Zyxir's Emacs configuration.")

(defvar zy-module-time-alist '()
  "Alist of module paths with their loading time.
Loading time (as number of milliseconds) is only recorded when
`init-file-debug' is non-nil or when the \"DEBUG\" environment
variable is set.")

(defun zy-load-rel (relpath &rest args)
  "Load the file in relative path RELPATH.
RELPATH is relative to `user-emacs-directory', and is formatted
with ARGS like `format' does.

The file is always loaded as byte code. If not, issue an error.

This function is and should only be used in the initialization
file (init.el)."
  (let ((abspath (concat (expand-file-name
                          (apply #'format relpath args)
                          user-emacs-directory)
                         ".elc")))
    (condition-case err
        ;; Benchmark the loading while debugging.
        (let* ((form `(load ,abspath nil 'nomessage 'nosuffix)))
          (if init-file-debug
              (let* ((start-time (current-time))
                     (_ (eval form))
                     (elapsed (time-since start-time))
                     (msecs (* 1000 (float-time elapsed))))
                (message "Loading %s...took %.3f milliseconds" abspath msecs)
                (add-to-list 'zy-module-time-alist
                             (cons (file-name-base abspath) msecs)))
            (eval form)))
      (file-missing
       (error "`%s' encountered while loading %s: %s"
              (car err) abspath (cdr err))))))

(let (;; `file-name-handler-alist' is consulted each time a file is loaded.
      ;; Unsetting it speeds up startup notably.
      (file-name-handler-alist nil)
      ;; `load-source-file-function' decides what function to be called to do
      ;; code conversion before reading a source file. Since I do not need code
      ;; conversion while loading the configuration, unsetting it reduces tons
      ;; of startup time.
      (load-source-file-function nil)
      ;; Don't spend precious time checking modified time during startup.
      ;; Ensuring that the configuration is updated is the job of zy-sync.el.
      ;; Run "emacs --sync" to perform a synchronization of the configuration
      ;; before startup.
      (load-prefer-newer nil))

  ;; Load all components of Zylib manually, which defines utility functions and
  ;; macros that most parts of this configuration depend on. Load them by path
  ;; manually reduces the tiny bit of time used to locate them in `load-path'.
  (zy-load-rel "lisp/zylib-core")
  (zy-load-rel "lisp/zylib-pkg")
  (zy-load-rel "lisp/zylib-key")
  (zy-load-rel "lisp/zylib")

  ;; Load the custom file now.
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file nil (not init-file-debug) 'nosuffix)

  ;; Load all modules in order.
  (dolist (module zy-modules)
    (zy-load-rel "modules/zy-%s"
                 (substring (symbol-name module) 1))))

;; Sort `zy-module-time-alist'.
(when zy-module-time-alist
  (setq zy-module-time-alist
        (sort zy-module-time-alist
              (lambda (pair1 pair2)
                (>= (cdr pair1) (cdr pair2))))))

(provide 'init)

;;; init.el ends here
