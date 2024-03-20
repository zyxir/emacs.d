;;; init-util.el --- Lisp utility.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file contains useful Lisp functions and macros which will be further
;; used in the config.

;;; Code:

(require 'init-elpa)

;; Load Zylib.
(require-package '(zylib "zylib.el"))

;;;; Symbol Manipulation

(defun zy/unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

;;;; Hook Management

(defun zy/-resolve-hook-forms (hooks)
  "Convert a list of modes into a list of hook symbols.

HOOKS is either an unquoted mode, an unquoted list of modes, a
quoted hook variable or a quoted list of hook variables."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (zy/unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook"
                                            (symbol-name hook)))))))

(defmacro add-hook! (hooks &rest rest)
  "A convennient macro to add N functions to M hooks.

HOOKS is either an unquoted mode, an unquoted list of modes, a
quoted hook variable or a quoted list of hook variables.

REST can contain optional properties :local, :append, and/or
:depth [N], which will make the hook buffer-local or append to
the list of hooks (respectively).

The rest of REST are the function(s) to be added: this can be a
quoted function, a quoted list thereof, a list of `defun' or
`cl-defun' forms, or arbitrary forms (will implicitly be wrapped
in a lambda).

This function was adapted from Doom Emacs."
  (declare (indent 1) (debug t))
  (let* ((hook-forms (zy/-resolve-hook-forms hooks))
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

(defmacro setq-hook! (hooks &rest rest)
  "Use `setq-local' on REST after HOOKS."
  (declare (indent 1) (debug t))
  `(add-hook! ,hooks (setq-local ,@rest)))

(provide 'init-util)

;; Advice Management

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'.  WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'.  DOCSTRING and BODY are as in
`defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING \
&rest [WHERE PLACES...] BODY)

This function is based on `defadvice!' of Doom Emacs."
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) ',symbol))))))

;; This is copied from Doom Emacs.
(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Undefine an advice called SYMBOL.

This has the same signature as `defadvice!' an exists as an easy
undefiner when testing advice (when combined with `rotate-text').

\(fn SYMBOL ARGLIST &optional DOCSTRING \
&rest [WHERE PLACES...] BODY)

This function is based on `undefadvice!' of Doom Emacs."
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))

;;;; More Lazy Loading Macros

(defun zy/-gen-after-load (features body)
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

(defun zy/-normalize-features (features)
  "Normalize FEATURES for macro expansion.
FEATURES can be:

- A quoted list of symbols.
- An unquoted list of symbols.
- A quoted symbol.
- An unquoted symbol.

This function always returns an unquoted list."
  (setq features (zy/unquote features))
  (if (listp features) features (list features)))

(defmacro after! (features &rest body)
  "Evaluate BODY after FEATURES are available.

FEATURES can be a symbol or a list of symbols. It can be quoted
or unquoted."
  (declare (indent 1) (debug (form def-body)))
  (zy/-gen-after-load (zy/-normalize-features features) body))

(defun zy/-gen-deferred-features (features)
  "Generate deferred loading statements for FEATURES.
FEATURES is a list of feature symbols.

If `daemonp' returns non-nil, FEATURES will be loaded right now.
Otherwise, they will be executed at `window-setup-hook'."
  (let* ((loading-sexps (mapcar
                         (lambda (x) `(require ',(zy/unquote x)))
                         features)))
    `(if (daemonp)
         ,(macroexp-progn loading-sexps)
       (add-hook 'window-setup-hook
                 (lambda () ,@loading-sexps)))))

(defmacro after-deferred! (features &rest body)
  "Defer FEATURES, and evaluate BODY after them.
If running in daemon mode, require FEATURES now, and BODY will be
evaluated subsequently; otherwise require FEATURES at
`window-setup-hook'.

FEATURES can be a symbol or a list of symbols. It can be quoted
or unquoted."
  (declare (indent 1) (debug (form def-body)))
  (let ((features (zy/-normalize-features features)))
    `(progn
       ,(zy/-gen-deferred-features features)
       ,(zy/-gen-after-load features body))))

(defun zy/-gen-maybe-required-features (features)
  "Generate maybe required statements for FEATURES.
FEATURES is a list of feature symbols.

If `daemonp' returns non-nil, FEATURES will be loaded right now.
Otherwise, do nothing."
  (let* ((loading-sexps (mapcar
                         (lambda (x) `(require ',(zy/unquote x)))
                         features)))
    `(when (daemonp) ,(macroexp-progn loading-sexps))))

(defmacro after-or-now! (features &rest body)
  "Evaluate BODY after FEATURES are available.
If running in daemon mode, require FEATURES now, and BODY will be
evaluated subsequently.

FEATURES can be a symbol or a list of symbols. It can be quoted
or unquoted.

This is similar to `after-deferred!', but FEATURES will not be
automatically required if not in daemon mode."
  (declare (indent 1) (debug (form def-body)))
  (let ((features (zy/-normalize-features features)))
    `(progn
       ,(zy/-gen-maybe-required-features features)
       ,(zy/-gen-after-load features body))))

(defmacro defer! (&rest body)
  "Run BODY at `window-setup-hook'.
If running in daemon mode, run them now."
  (declare (indent 0) (debug (form def-body)))
  `(if (daemonp)
       ,(macroexp-progn body)
     (add-hook 'window-setup-hook
               (lambda () ,@body))))

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

;;;; Filesystem

(defun zy/first-existing-path (&rest paths)
  "Return the first existing path in PATHS."
  (cl-some (lambda (path)
             (if (file-exists-p path) path nil))
           paths))

;;; init-util.el ends here
