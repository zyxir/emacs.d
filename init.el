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
    +roam
    +lingual
    +bib
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

(defun zy-load-rel (file)
  "Load FILE silently, efficiently and safely.

If FILE is a relative path, interpret it as a path relative to
the \"lisp\" directory in `user-emacs-directory', without suffix.
The suffix \".elc\" is always appended to FILE, meaning that
always try to load the byte code regardless of whether it exists.

If FILE is a absolute path, load it directly. This is the only
case where the file is loaded as is, without a guessed or
appended suffix. This case is reserved for loading the custom
file.

If FILE is a symbol whose name starts with the plus sign, like
`+leader', interpret it as a module of this configuration, and
load it accordingly. Always load a module's byte code.

If any error occur during loading, the error is captured and
reported, therefore the remaining loading keeps going on.

Return the full path of the file actually loaded.

This function is and should only be used in the initialization
file (init.el)."
  (let* ((path (cond
                ;; A path.
                ((stringp file)
                 (if (file-name-absolute-p file)
                     ;; An absolute path.
                     file
                   ;; A Lisp library.
                   (concat (expand-file-name
                            file
                            (expand-file-name "lisp" user-emacs-directory))
                           ".elc")))
                ;; A module.
                ((and (symbolp file)
                      (string-prefix-p "+" (symbol-name file)))
                 (expand-file-name
                  (format "zy-%s.elc"
                          (substring (symbol-name file) 1))
                  (expand-file-name "modules" user-emacs-directory)))
                ;; Unknown cases.
                (t (lwarn 'init :warning
                          "I don't know how to load `%s'" file)))))
    (condition-case err
        (load path nil 'nomessage 'nosuffix)
      (error
       (lwarn 'init :error
              "Loading %s: %s" path err))
      (:success path))))

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

  ;; Load the `+bench' module first to benchmark the loading of files.
  (zy-load-rel '+bench)

  ;; Load all components of Zylib manually, which defines utility functions and
  ;; macros that most parts of this configuration depend on. Load them by path
  ;; manually reduces the tiny bit of time used to locate them in `load-path'.
  (zy-load-rel "zylib-core")
  (zy-load-rel "zylib-pkg")
  (zy-load-rel "zylib-key")
  (zy-load-rel "zylib")

  ;; Load the custom file now.
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (zy-load-rel custom-file)

  ;; Load all modules in order.
  (dolist (module zy-modules)
    (zy-load-rel module)))

;; Sort `zy-module-time-alist'.
(when zy-module-time-alist
  (setq zy-module-time-alist
        (sort zy-module-time-alist
              (lambda (pair1 pair2)
                (>= (cdr pair1) (cdr pair2))))))

(provide 'init)

;;; init.el ends here
