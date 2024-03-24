;;; zylib-core.el --- Emacs Lisp syntax sugars. -*- lexical-binding: t -*-
;;; Commentary:

;; This file provides syntax sugars for writting Emacs Lisp code. Many
;; variables, functions and macros of this file are either copied or adapted
;; from famous repositories like Doom Emacs.

;;; Code:

(eval-when-compile (require 'cl-lib))

;;;; Platform Detection

(defvar zy-platform
  (cond ((memq system-type '(ms-dos windows-nt cygwin))
	 'windows)
	((eq system-type 'gnu/linux)
	 (if (getenv "WSL_DISTRO_NAME") 'wsl 'linux))
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

(defun zy--gen-after-load (features body)
  "Generate `eval-after-load' statements to represent FEATURES.
FEATURES is a list of feature symbols and BODY is the body to be
lazy loaded.

ALl features will be required at compile time to silence compiler
warnings."
  (let* ((require-sexp `(eval-and-compile
                          ,@(mapcar #'(lambda (x) `(require ',x))
                                    features)))
         (body `(lambda () ,require-sexp ,@body)))
    (dolist (feature features)
      (setq body `(eval-after-load ',feature ,body)))
    body))

(defun zy--normalize-features (features)
  "Normalize FEATURES for macro expansion.
FEATURES can be:

- A quoted list of symbols.
- A quoted symbol.

This function always returns an unquoted list of unquoted
symbols.

If there is any symbol starting with the plus sign, like
`+leader' does, it is recognized as a module of Zyxir's Emacs
configuration, and is converted to its feature name accordingly,
like `+leader' is converted to `zy-leader'."
  ;; Unquote the form.
  (setq features (unquote! features))
  ;; Listify the form.
  (unless (listp features)
    (setq features (list features)))
  ;; Unquote and normalize elements.
  (mapcar (lambda (x)
            (setq x (unquote! x))
            (if (string-prefix-p "+" (symbol-name x))
                (intern (format "zy-%s" (substring (symbol-name x) 1)))
              x))
          features))

(defmacro after! (features &rest body)
  "Evaluate BODY after FEATURES are available.

FEATURES can be a symbol or a list of symbols. It can be quoted
or unquoted. If a symbol starts with the plus sign like `+leader'
does, it is considered a module of the configuration, and will be
processed accordingly."
  (declare (indent 1) (debug (form def-body)))
  (zy--gen-after-load (zy--normalize-features features) body))

(defmacro after-gui! (&rest body)
  "Run BODY when GUI is ready.
In BODY, one can use the variable FRAME, which is bound to the
newly created GUI frame."
  (declare (indent 0) (debug (form def-body)))
  (let* ((fn-name (intern (format "after-gui-%s" (cl-gensym))))
         (fn-form `(defun ,fn-name (&optional frame)
                     (if (display-graphic-p frame)
                         ;; When GUI is ready, remove the hook function and
                         ;; execute BODY.
                         (progn
                           (remove-hook 'after-make-frame-functions
                                        ',fn-name)
                           ,@body)
                       ;; Otherwise, add itself to the hook.
                       (add-hook 'after-make-frame-functions
                                 ',fn-name)))))
    `(progn ,fn-form (,fn-name))))

(defmacro daemon-require! (&rest features)
  "Require FEATURES if running as a daemon.
Each FEATURE in FEATURES is a quoted feature symbol.

\(fn [FEATURE] ...)"
  (let* ((require-forms
          (mapcar (lambda (feature)
                    `(require ',(unquote! feature)))
                  features)))
    `(when (daemonp) ,@require-forms)))

;;;; Filesystem

(defun some-path! (&rest paths)
  "Return the first existing path in PATHS."
  (cl-some (lambda (path)
             (if (file-exists-p path) path nil))
           paths))

(provide 'zylib-core)

;;; zylib-core.el ends here
