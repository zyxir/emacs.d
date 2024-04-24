;;; zylib-core.el --- Emacs Lisp syntax sugars. -*- lexical-binding: t -*-
;;; Commentary:

;; This file provides syntax sugars for writting Emacs Lisp code. Many
;; variables, functions and macros of this file are either copied or adapted
;; from famous repositories like Doom Emacs.

;;; Code:

(eval-when-compile (require 'cl-lib))

;;;; Platform Detection

(defconst zy--wsl-p
  (eval-and-compile
    (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop"))
  "Whether Emacs is running on WSL.

WSL, which stands for Windows Subsystem for Linux, is a special
kind of Linux running on top of Microsoft Windows. There are
several ways to detect if the current system is a WSL, as
explained in question #1749781 in Stack Exchange. The fastest
approach is detecting a environment variable like
\"WSL_DISTRO_NAME\", but it is unreliable when running as a
daemon. Detecting by the existence of the interop file is by far
the most reliable way according to the answer.

Checking the existence of a file may slow down startup for
several milliseconds, so we detect it at compile time via
`eval-and-compile', since if a system is a WSL at compile time,
it should remains a WSL at runtime.

Do not use this constant directly. Use `zy-platform' instead.")

(defvar zy-platform
  (cond ((memq system-type '(ms-dos windows-nt cygwin))
	 'windows)
	((eq system-type 'gnu/linux)
	 (if zy--wsl-p 'wsl 'linux))
	(t 'unsupported))
  "The platform (operating system) Emacs is running on.
Possible values:
  `windows'     Microsoft Windows
  `wsl'         Windows subsystem for Linux
  `linux'       a Linux distribution
  `unsupported' an unsupported platform")

;;;; Feature Detection

(defun native-comp-available-p! ()
  "Return non-nil if native compilation is available.

The function `native-comp-available-p' is strange: If Emacs is
not built with native compilation, it is just absent, and calling
it will result in an error. This function wraps around
`native-comp-available-p' as a less error-prone way to detect the
availability of native compilation."
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p)))

(defun zy--require-when-compile (flist)
  "Require every feature in FLIST during byte compilation.
This is only used internally in Zylib."
  (when (bound-and-true-p byte-compile-current-file)
    (seq-map #'require flist)))

;;;; Module Management

(defun featurify! (symbol)
  "Convert SYMBOL into a feature symbol.

If SYMBOL is a module symbol which starts with `+', like
`+quickins' or `+font', it is converted into its feature symbol,
like `zy-quickins' and `zy-font'.

Otherwise SYMBOL is returned as is."
  (let ((symbol-name (symbol-name symbol)))
    (if (string-prefix-p "+" symbol-name)
        (intern
         (format "zy-%s" (substring symbol-name 1)))
      symbol)))

(defun modulep! (symbol)
  "Determine if SYMBOL is a enabled module.
Return non-nil if the module associated with SYMBOL is an enabled
module of Zyxir's Emacs configuration. SYMBOL should be a symbol
started with `+', like `+quickins' or `+font'.

A module being enabled does not necessarily mean it being loaded.
This function returns non-nil even before the module is loaded."
  (and
   ;; `zy-modules' is defined in init.el, tracking all enabled modules.
   (boundp 'zy-modules)
   (memq symbol zy-modules)))

(defun disabled-modules! (&optional form)
  "Return the list of disabled modules.

The 2nd optional argument FORM specifies how to represent these
modules. It can be:

  nil       module symbols (like `+leader')
  `feature' feature symbols (like `zy-leader')
  `feat'    same as `feature'
  `path'    filesystem paths"
  (let* ((all-modules
          (seq-map (lambda (file)
                     (let* ((sans-ext (string-remove-prefix "zy-" file))
                            (mod-name (string-remove-suffix ".el" sans-ext))
                            (mod-symbol (intern (format "+%s" mod-name))))
                       mod-symbol))
                   (directory-files
                    (expand-file-name "modules" user-emacs-directory)
                    nil "^zy-.*\\.el$")))
         (enabled-modules
          (if (boundp 'zy-modules)
              zy-modules
            (error "Cannot get the list of enabled modules")))
         (disabled-modules (cl-set-difference all-modules enabled-modules)))
    (cond
     ((not form) disabled-modules)
     ((memq form '(feat feature))
      (seq-map (lambda (module)
                 (intern (format "zy-%s" (substring (symbol-name module) 1))))
               disabled-modules))
     ((eq form 'path)
      (seq-map (lambda (module)
                 (expand-file-name
                  (format "modules/zy-%s.el"
                          (substring (symbol-name module) 1))
                  user-emacs-directory))
               disabled-modules))
     (t (error "Invalid `form' argument: %s" form)))))

;;;; Symbol Manipulation

(defun unquote! (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

;;;; Hook Management

(defmacro add-hook! (hooks &rest rest)
  "A convennient macro to add N functions to M hooks.

HOOKS is either a quoted hook variable or a quoted list of hook
variables.

REST can contain optional properties :local, :append, and/or
:depth [N], which will make the hook buffer-local or append to
the list of hooks (respectively).

The rest of REST are the function(s) to be added: this can be a
quoted function, a quoted list thereof, a list of `defun' or
`cl-defun' forms, or arbitrary forms (will implicitly be wrapped
in a lambda).

This function was adapted from Doom Emacs."
  (declare (indent 1) (debug t))
  (let* ((hook-forms (if (eq (car-safe hooks) 'quote)
                         (ensure-list (cadr hooks))
                       (error "HOOKS must be quoted")))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'quote (cadr next)))
                    (t (prog1 `(lambda (&rest _) ,@(cons next rest))
                         (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook ',hook-forms)
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenient macro for removing N functions from M hooks.

HOOKS and REST are the same as in `add-hook!'.

This function is based on `remove-hook!' of Doom Emacs."
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))


;;;; Lazy Loading Macros

(defun zy--gen-after-load (features* body)
  "Generate statements to load BODY after FEATURES.
FEATURES* is a normalized nested feature list returned by
`zy--process-features'.

The list can contain keywords `:any' and `:all', where no
keywords implied `:all'.

ALl features will be required at compile time to silence compiler
warnings."
  (cond
   ((and features* (symbolp features*))
    `((eval-after-load ',features* ',(macroexp-progn body))))
   ((and (consp features*) (memq (car features*) '(:or :any)))
    (cl-mapcan #'(lambda (x) (zy--gen-after-load x body)) (cdr features*)))
   ((and (consp features*) (memq (car features*) '(:and :all)))
    (cl-dolist (next (cdr features*))
      (setq body (zy--gen-after-load next body)))
    body)
   ((listp features*)
    (zy--gen-after-load (cons :all features*) body))))

(defun zy--process-features (features*)
  "Convert FEATURES* for macro expansion.
FEATURES* can be:

- A quoted symbol.
- A quoted nested list of symbols.

The list can contain keywords `:any' and `:all', where no
keywords implied `:all'.

If there is any symbol starting with the plus sign, like
`+leader' does, it is recognized as a module of Zyxir's Emacs
configuration, and is converted to its feature name accordingly,
like `+leader' is converted to `zy-leader'.

This function returns (NORMALIZED FLIST), where:

- NORMALIZED is the normalized FERATURES*, where there is no
  quote and every symbol is converted.

- FLIST is the flattened list of features. All keywords are
excluded."
  ;; Remove the quote.
  (setq features* (cadr features*))
  ;; Normalize and flatten elements.
  (cl-labels ((normalize (symbol)
                (featurify! symbol))
              (normalize-recursively (features*)
                (if (listp features*)
                    (mapcar #'normalize-recursively features*)
                  (normalize features*)))
              (flatten-recursively (normalized)
                (if (listp normalized)
                    (mapcan #'flatten-recursively normalized)
                  (unless (keywordp normalized)
                    (list normalized)))))
    (let* ((normalized (normalize-recursively features*))
           (flist (flatten-recursively normalized)))
      (list normalized flist))))

(defmacro after! (features* &rest body)
  "Evaluate BODY after FEATURES* are available.

FEATURES* can be a symbol or a nested list of symbols. It should
be quoted. If a symbol starts with the plus sign like `+leader'
does, it is considered a module of the configuration, and will be
processed accordingly. It can contain keywords like `:all' and
`:any'."
  (declare (indent 1) (debug (form def-body)))
  (let* ((result (zy--process-features features*))
         (features* (car result))
         (flist (cadr result)))
    ;; While byte-compiling the file, require all features so that all its
    ;; symbols are in scope, like `use-package' does.
    (zy--require-when-compile flist)
    ;; Generate the `eval-after-load' statement.
    (macroexp-progn
     (zy--gen-after-load features* body))))

(defmacro after-frame! (&rest body)
  "Run BODY when the first frame is created.
In BODY, although not suggested, one can use the variable FRAME,
which is bound to the newly created frame."
  (declare (indent 0) (debug (form def-body)))
  (let* ((fn-name (cl-gensym "after-frame!-"))
         (fn-form `(defun ,fn-name (&optional _) ,@body t)))
    `(if (daemonp)
         (add-hook 'after-make-frame-functions ,fn-form)
       ,@body)))

(defmacro after-graphics! (&rest body)
  "Run BODY when graphics is ready.
In BODY, although not suggested, one can use the variable FRAME,
which is bound to the newly created graphic frame."
  (declare (indent 0) (debug (form def-body)))
  (let* ((fn-name (cl-gensym "after-graphics!-"))
         (fn-form `(defun ,fn-name (&optional frame)
                     (if (display-graphic-p frame)
                         ;; When GUI is ready, remove the hook function and
                         ;; execute BODY.
                         (progn
                           (remove-hook 'after-make-frame-functions
                                        ',fn-name)
                           ,@body)
                       ;; Otherwise, add itself to the hooks.
                       (add-hook 'after-make-frame-functions
                                 ',fn-name))
                     ;; Return something to prevent compiling issues.
                     t)))
    `(add-hook 'window-setup-hook ,fn-form)))

(defmacro daemon-require! (&rest features)
  "Require FEATURES if running as a daemon.
Each FEATURE in FEATURES is a quoted feature symbol.

\(fn [FEATURE] ...)"
  (let* ((require-forms
          (mapcar (lambda (feature)
                    `(require ,feature))
                  features)))
    `(when (daemonp) ,@require-forms)))

;;;; Filesystem

(defun some-path! (&rest paths)
  "Return the first existing path in PATHS."
  (cl-some (lambda (path)
             (if (file-exists-p path) path nil))
           paths))

;;;; Color manipulation

(defun zy--lighten-color (color percent &optional darken)
  "Lighten a color tag COLOR by PERCENT.

Return the lightened color tag. If optional argument DARKEN is
non-nil, return the darkened color instead."
  (eval-and-compile (require 'color))
  (let* ((rgb (color-name-to-rgb color))
         (hsl (apply #'color-rgb-to-hsl rgb))
         (hsl (apply (if darken #'color-darken-hsl #'color-lighten-hsl)
                     (append hsl `(,percent))))
         (rgb (apply #'color-hsl-to-rgb hsl))
         (color (apply #'color-rgb-to-hex (append rgb '(2)))))
    color))

(defun lighten! (color percent)
  "Return a color tag of COLOR lightened by PERCENT."
  (zy--lighten-color color percent))

(defun darken! (color percent)
  "Return a color tag of COLOR darkened by PERCENT."
  (zy--lighten-color color percent 'darken))

(provide 'zylib-core)

;;; zylib-core.el ends here
