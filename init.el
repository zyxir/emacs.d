;;; init.el --- the main config -*- lexical-binding: t -*-

;; Copyright (C) 2022 Eric Zhuo Chen

;; Author: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Maintainer: Eric Zhuo Chen <zyxirchen@outlook.com>
;; Created: 2022-10-28


;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file is the main part of my configuration.

;; The load order of Emacs is as follows:

;;   > early-init.el
;;   > init.el
;;   > hook: `after-init-hook'
;;   > hook: `emacs-startup-hook'
;;   > hook: `window-setup-hook'
;;   > After stratup:
;;     > First user input: `zy-first-input-hook'
;;     > First buffer visit: `zy-first-buffer-hook'
;;     > First file visit: `zy-first-file-hook'

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'cl-lib)

;;;; Base settings

;;;;; Make startup faster

;; These settings are needed prior to everything else.

;;;;;; Check minimum version

;; Check version at both compile and runtime.
(eval-and-compile
  ;; The minimum version to run this configuration is 28.1.
  (when (< emacs-major-version 28)
    (user-error
     "Emacs version is %s, but this config requires 28.1 or newer"
     emacs-version)))

;;;;;; Adjust garbage collection

;; Garbage collection can occur many times during startup, which slows things
;; down.  As is suggested by a lot of users (and by the official documentation
;; of `gc-cons-threshold'), you can increase the threshold temporarily to
;; inhibit garbage collection at startup.  Besides, the 800 KB default is too
;; low by modern standards, so it is set to a higher value even after startup.

(let ((init-gc-cons-threshold (* 256 1024 1024))
      (normal-gc-cons-threshold (* 20 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold normal-gc-cons-threshold))))

;;;;;; Unset file name handlers

;; This technique is borrowed from Doom Emacs.  `file-name-handler-alist' is
;; consulted on each call to `require', `load', or various file/io functions
;; (like `expand-file-name' or `file-remote-p').  Setting this value to nil
;; helps reducing startup time a lot.

(let ((old-value (default-toplevel-value 'file-name-handler-alist)))
  (setq file-name-handler-alist
        ;; If the bundled elisp for this Emacs install isn't byte-compiled (but
        ;; is compressed), then leave the gzip file handler there so Emacs won't
        ;; forget how to read read them.
        ;;
        ;; calc-loaddefs.el is our heuristic for this because it is built-in to
        ;; all supported versions of Emacs, and calc.el explicitly loads it
        ;; uncompiled. This ensures that the only other, possible fallback would
        ;; be calc-loaddefs.el.gz.
        (if (eval-when-compile
              (locate-file-internal "calc-loaddefs.el" load-path))
            nil
          (list (rassq 'jka-compr-handler old-value))))
  ;; Make sure the new value survives any current let-binding.
  (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
  ;; Remember the old value so that it can be used when needed.
  (put 'file-name-handler-alist 'initial-value old-value)
  ;; Restore it after startup.
  (add-hook 'emacs-startup-hook
            (defun zy--reset-file-handler-alist-h ()
              (setq file-name-handler-alist
                    ;; Merge instead of overwrite in case it is modified during
                    ;; startup.
                    (delete-dups
                     (append file-name-handler-alist old-value))))
            -99))

;;;;;; Reduce GUI noises

;; Reduce some GUI noises can also reduce startup time.

(unless noninteractive
  ;; Frame resizing caused by font changing can increase startup time
  ;; dramatically.  Setting this value to t stops that from happening.
  (setq frame-inhibit-implied-resize t)

  ;; Do not show the startup screen.
  (setq inhibit-startup-screen t)

  ;; Show startup time instead of "For information about ...".

  (defun display-startup-echo-area-message ()
    "Display startup time."
    (message "Emacs ready in %.2f seconds."
             (float-time
              (time-subtract (current-time) before-init-time))))

  ;; Even if `inhibit-startup-screen' is set to t, it would still initialize
  ;; anyway.  This involves some file IO and/or bitmap work.  It should be
  ;; banned completly.
  (advice-add #'display-startup-screen :override #'ignore)

  ;; Start the scratch buffer in `fundamental-mode' with no additional text.
  ;; This is cleaner and saves some time.
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  (unless init-file-debug
    ;; Site files tend to use `load-file', which emits "Loading X..."  messages
    ;; in the echo area.  Writing to the echo-area triggers a redisplay, which
    ;; can be expensive during startup.  This may also cause an flash of white
    ;; when creating the first frame.
    (advice-add #'load-file :override
                (defun load-file-silently-a (file)
                  (load file nil 'nomessage)))
    ;; And disable this advice latter
    (add-hook 'emacs-startup-hook
              (defun zy--restore-load-file-h ()
                (advice-remove #'load-file 'load-file-silently-a)))

    ;; Disabling the mode line also reduces startup time by approximately 30 to
    ;; 50 ms, according to Doom Emacs.
    (put 'mode-line-format 'initial-value
         (default-toplevel-value 'mode-line-format))
    (setq-default mode-line-format nil)
    (dolist (buf (buffer-list))
      (with-current-buffer buf (setq mode-line-format nil)))
    (add-hook 'after-init-hook
              (defun zy--reset-modeline-format-h ()
                (unless (default-toplevel-value 'mode-line-format)
                  (setq-default mode-line-format
                                (get 'mode-line-format 'initial-value)))))

    ;; Redisplays during startup cost time and produce ugly flashes of unstyled
    ;; Emacs.  However, if any error occurs during startup, Emacs could appear
    ;; frozen or garbled.
    (setq-default inhibit-redisplay t
                  inhibit-message t)
    (add-hook 'after-init-hook
              (defun zy--reset-inhibited-vars-h ()
                (setq-default inhibit-redisplay nil
                              inhibit-message nil))
              (redraw-frame))

    ;; Even if the toolbar is explicitly disabled in early-init.el, it is still
    ;; populated regardless.  So it should be lazy-loaded until `tool-bar-mode'
    ;; is actually called.
    (advice-add #'tool-bar-setup :override #'ignore)
    (advice-add #'tool-bar-mode :before
                (defun zy--setup-toolbar-a (&rest _)
                  (tool-bar-setup)
                  (advice-remove #'tool-bar-mode 'zy--setup-toolbar-a)))))

;;;;; Define common functions, variables, hooks

;;;;;; Additional functions & macros

;; These functions and macros make writting this configuration much easier.
;; Many of them are copied or adapted from Doom Emacs.

;;;;;;; Logging

(defvar zy-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    map)
  "Keymap for Zyxir's Log mode.")

(define-derived-mode zy-log-mode special-mode "Log"
  "Major mode for viewing logs.
Commands:
\\{zy-log-mode-map}")

(defvar zy-log-buffer
  (with-current-buffer (get-buffer-create "*Log*")
    (zy-log-mode)
    (current-buffer))
  "Buffer for log messages.")

(defun zy--log (module text &rest args)
  "Log a message TEXT in the *Log* buffer.
ARGS is arguments used to format TEXT.

The message is prettified, and combined with additional
information (including MODULE, the module that is logging the
message), before being written to the *Log* buffer."
  (let* (;; Time since initialization.
         (time (format "%.06f" (float-time (time-since before-init-time))))
         ;; The module indicator.
         (module (format ":%s:" (if (symbolp module)
                                    (symbol-name module)
                                  module)))
         ;; Padding between each printed line.
         (padding (make-string (+ (length time) (length module) 2) ?\s))
         ;; The formatted text.
         (text (apply 'format text args))
         ;; Split `text' into segments by newlines.
         (text-segs (delete "" (split-string text "\n"))))
    ;; Print all text segments into the *Log* buffer.
    (with-current-buffer zy-log-buffer
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (concat
                 (propertize time 'face 'font-lock-doc-face)
                 " "
                 (propertize module 'face 'font-lock-keyword-face)
                 " "
                 (car-safe text-segs)
                 "\n"))
        (when (cdr-safe text-segs)
          (dolist (seg (cdr text-segs))
            (insert (concat padding seg "\n"))))))))

(defmacro zy-log (module message &rest args)
  "Log MESSAGE formatted with ARGS in *Messages*.
MODULE is the module that emits the message.

If `init-file-debug' is nil, do nothing."
  (declare (debug t))
  `(when init-file-debug (zy--log ,module ,message ,@args)))

;;;;;;; Symbol manipulation

;; This is copied from Doom Emacs.
(eval-and-compile
  (defun zy-unquote (exp)
    "Return EXP unquoted."
    (declare (pure t) (side-effect-free t))
    (while (memq (car-safe exp) '(quote function))
      (setq exp (cadr exp)))
    exp))

;; This is copied from Doom Emacs.
(defun zy-keyword-intern (str)
  "Convert STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

;;;;;;; Enhanced hook system

(define-error 'hook-error "Error in a hook.")

;; This is copied from Doom Emacs.
(defvar zy--hook nil
  "Currently triggered hook.")

;; This is copied from Doom Emacs.
(defun zy-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.

Should be used with `run-hook-wrapped'."
  (zy-log 'hook "Hook %s: run %s" (or zy--hook '*) hook)
  (condition-case-unless-debug e
      (funcall hook)
    (error
     (signal 'hook-error (list hook e))))
  ;; Return nil to keep `run-hook-wrapped' running.
  nil)

;; This is copied from Doom Emacs.  Use this on custom hooks only, otherwise
;; *Log* will be filled with arbitrary hook messages.
(defun zy-run-hooks (&rest hooks)
  "Run HOOKS with better error handling.

HOOKS is a list of hook variable symbols.  This is used as an
advice to replace `run-hooks'."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (let ((zy--hook hook))
          (run-hook-wrapped hook #'zy-run-hook))
      (hook-error
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (if (symbolp (cadr e))
                    (symbol-name (cadr e))
                  (cadr e))
                (caddr e)))
       (signal 'hook-error (cons hook (cdr e)))))))

;; This is copied from Doom Emacs.
(defun zy-run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to run on TRIGGER-HOOKS.

HOOK-VAR is to be invoked exactly once when any of the
TRIGGER-HOOKS are invoked *after* Emacs has initialized (to
reduce false positives).  Once HOOK-VAR is triggered, it is reset
to nil."
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "chain-%s-to-%s-h" hook-var hook)))
          running-p)
      (fset fn
            (lambda (&rest _)
              ;; Only trigger this after Emacs has initialized.
              (when (and after-init-time
                         (not running-p)
                         (or (daemonp)
                             ;; In some cases, hooks may be lexically unset to
                             ;; inhibit them during expensive batch operations
                             ;; on buffers (such as when processing buffers
                             ;; internally).  In these cases we should assume
                             ;; this hook wasn't invoked interactively.
                             (and (boundp hook)
                                  (symbol-value hook))))
                (setq running-p t)  ; prevent infinite recursion
                (zy-run-hooks hook-var)
                (set hook-var nil))))
      (cond ((daemonp)
             ;; Do not lazy load in a daemon session.
             (add-hook 'after-init-hook fn 'append))
            ((eq hook 'find-file-hook)
             ;; Advice `after-find-file' instead of using `find-file-hook'
             ;; because the latter is triggered too late (after the file has
             ;; opened and modes are all set up).
             (advice-add 'after-find-file :before fn '((depth . -99))))
            (t (add-hook hook fn -99)))
      fn)))

;;;;;;; Macros as syntactic sugars

;; This is copied from Doom Emacs.
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first
invoked, then never again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted
function (which will be advised)."
  (declare (indent 1))
  (let ((append-p (if (eq (car forms) :after) (pop forms)))
        (fn (gensym "zy-transient-hook")))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S." (zy-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym ',fn))
                 ((symbolp sym)   (remove-hook sym ',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function
                          ,(if append-p :after :before) ',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym ',fn ,append-p))))))

;; This is copied from Doom Emacs.
(defmacro setq! (&rest settings)
  "Setting variables according to SETTINGS.

Unlike `setq', this triggers custom setters on variables.  Unlike
`setopt', this won't needlessly pull in dependencies."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set)
                                  #'set-default-toplevel-value)
                              ',var ,val))))

;; This is copied from Doom Emacs.
(eval-and-compile
  (defun zy--resolve-hook-forms (hooks)
    "Convert a list of modes into a list of hook symbols.

HOOKS is either an unquoted mode, an unquoted list of modes, a
quoted hook variable or a quoted list of hook variables."
    (declare (pure t) (side-effect-free t))
    (let ((hook-list (ensure-list (zy-unquote hooks))))
      (if (eq (car-safe hooks) 'quote)
          hook-list
        (cl-loop for hook in hook-list
                 if (eq (car-safe hook) 'quote)
                 collect (cadr hook)
                 else collect (intern (format "%s-hook"
                                              (symbol-name hook))))))))

;; This is adapted from Doom Emacs.
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
in a lambda)."
  (declare (indent 1) (debug t))
  (let* ((hook-forms (zy--resolve-hook-forms hooks))
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

;; This is based on `remove-hook!' from Doom Emacs.
(defmacro remove-hook! (hooks &rest rest)
  "A convenient macro for removing N functions from M hooks.

HOOKS and REST are the same as in `add-hook!'."
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest rest)
  "Use `setq-local' on REST after HOOKS."
  (declare (indent 1) (debug t))
  `(add-hook! ,hooks (setq-local ,@rest)))

;; This is copied from Doom Emacs.
(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'.  WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'.  DOCSTRING and BODY are as in
`defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING \
&rest [WHERE PLACES...] BODY)"
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
&rest [WHERE PLACES...] BODY)"
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

;;;;;;; Operating system constants

(defconst *windows* (eq system-type 'windows-nt)
  "Non-nil if running on Windows.")

(defconst *linux* (eq system-type 'gnu/linux)
  "Non-nil if running on GNU/Linux.")

(defconst *posix* (if (memq system-type '(gnu/linux darwin)) t nil)
  "Non-nil if running on a (mostly) POSIX-complaint system.")

(defconst *wsl* (if (getenv "WSL_DISTRO_NAME") t nil)
  "Non-nil if running on WSL (Windows subsystem for Linux).")

;;;;;; Custom hooks

;;;;;;; First user input hook

(defcustom zy-first-input-hook nil
  "Hooks run before the first user input."
  :type 'hook
  :local 'permenant-local
  :group 'zyxir)

(zy-run-hook-on 'zy-first-input-hook
                '(pre-command-hook))

;;;;;;; Switch buffer/window/frame hook

(defvar zy-switch-buffer-hook nil
  "Hooks run after changing the current buffer.")

(defvar zy-switch-window-hook nil
  "Hooks run after changing the current window.")

(defvar zy-switch-frame-hook nil
  "Hooks run after changing the current frame.")

(defun zy-run-switch-buffer-hooks-h (&optional _)
  "Run all hooks in `zy-switch-buffer-hook'."
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'zy-switch-buffer-hook)))

(defvar zy--last-frame nil)
(defun zy-run-switch-window-or-frame-hooks-h (&optional _)
  "Run the two hooks if needed."
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks 'zy-switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks 'zy-switch-window-hook))))

;; Initialize these hooks after startup.
(add-hook! 'window-setup-hook
  (add-hook 'window-selection-change-functions
            #'zy-run-switch-window-or-frame-hooks-h)
  (add-hook 'window-buffer-change-function
            #'zy-run-switch-buffer-hooks-h)
  (add-hook 'server-visit-hook
            #'zy-run-switch-buffer-hooks-h))

;;;;;;; Load theme hook

(defvar zy-load-theme-hook nil
  "Hooks run after loading a theme.")

(defadvice! zy--run-load-theme-hook-a (&rest _)
  "Run hooks in `zy-load-theme-hook'."
  :after 'load-theme
  (zy-run-hooks 'zy-load-theme-hook))

;;;;;;; First buffer hook

(defcustom zy-first-buffer-hook nil
  "Hooks run before the first interactively opened buffer."
  :type 'hook
  :local 'permenant-local
  :group 'zyxir)

(zy-run-hook-on 'zy-first-buffer-hook
                '(dired-load-hook find-file-hook zy-switch-buffer-hook))

;;;;;;; First file hook

(defcustom zy-first-file-hook nil
  "Hooks run before the first interactively opened file."
  :type 'hook
  :local 'permenant-local
  :group 'zyxir)

(zy-run-hook-on 'zy-first-file-hook
                '(find-file-hook dired-initial-position-hook))

;;;;;; Incremental loading from Doom Emacs

;; I tried to design my own incremental loader at version 4.0, but it turned out
;; to be too complicated.  Finally I decided to adopt Doom Emacs's code.

;; This is copied from Doom Emacs.
(defvar zy-incremental-packages '(t)
  "A list of packages to load incrementally after startup.

Any large packages here may cause noticeable pauses, so it's
recommended you break them up into sub-packages.

Incremental loading does not occur in daemon sessions (they are
loaded immediately at startup).")

;; This is copied from Doom Emacs.
(defvar zy-incremental-first-idle-timer (if (daemonp) 0 2.0)
  "Incremental loading starts after this many seconds.")

;; This is adapted from Doom Emacs.
(defvar zy-incremental-idle-timer 0.75
  "Interval between two incremental loading processes.")

;; This is adapted from Doom Emacs.
(defun zy-load-packages-incrementally (packages &optional now)
  "Register PACKAGE to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally now, in
`zy-incremental-idle-timer' intervals."
  (let ((gc-cons-threshold most-positive-fixnum))
    (if (not now)
        ;; If `now' is nil, queue `packages' for loading.
        (cl-callf append zy-incremental-packages packages)
      ;; If `now' is non-nil, do the loading now.
      (while packages
        (let ((req (pop packages))
              idle-time)
          (if (featurep req)
              (zy-log 'iloader "Already loaded %s (%d left)"
                      req (length packages))
            (condition-case-unless-debug e
                (and
                 (or
                  ;; Don't load if not idle.
                  (null (setq idle-time (current-idle-time)))
                  ;; Don't load if the idle time is not enough.
                  (< (float-time idle-time) zy-incremental-first-idle-timer)
                  (not
                   ;; Interrupt the load once the user inputs something.
                   (while-no-input
                     (zy-log 'iloader "Loading %s (%d left)"
                             req (length packages))
                     (let ((inhibit-message t)
                           (file-name-handler-alist
                            (list (rassq 'jka-compr-handler
                                         file-name-handler-alist))))
                       ;; Ignore loading errors.
                       (require req nil 'noerror)
                       t))))
                 (push req packages))
              (error
               (message "Error: failed to incrementally load %S because %s"
                        req e)
               (setq packages nil)))
            (if (null packages)
                ;; If all queued packages are loaded, the job is finished.
                (zy-log 'iloader "Finished!")
              ;; Otherwise, pend the next load action.
              (run-at-time (if idle-time
                               zy-incremental-idle-timer
                             zy-incremental-first-idle-timer)
                           nil #'zy-load-packages-incrementally
                           packages t)
              ;; `packages' has been passed to the callback function, so safely
              ;; setting it to nil.
              (setq packages nil))))))))

(defun zy-load-packages-incrementally-h ()
  "Start loading packages incrementally.

Packages to be loaded are stored in `zy-incremental-packages'.
If this is a daemon session, load them all immediately instead."
  (when (numberp zy-incremental-first-idle-timer)
    (if (zerop zy-incremental-first-idle-timer)
        (mapc #'require (cdr zy-incremental-packages))
      (run-with-idle-timer zy-incremental-first-idle-timer
                           nil #'zy-load-packages-incrementally
                           (cdr zy-incremental-packages) t))))

(add-hook 'window-setup-hook #'zy-load-packages-incrementally-h)

;;;;; Install third-party base utilities

;;;;;; Use Straight as package manager

(setq-default
 ;; Cache autoloads into a single file to speed up startup.
 straight-cache-autoloads t
 ;; Use different build directories for different versions of Emacs to cope with
 ;; byte code incompatibility.
 straight-build-dir (format "build-%s" emacs-version)
 ;; Do not check for modifications, until explicitly asked to.
 straight-check-for-modifications '(find-when-checking))

(defvar bootstrap-version)
(or (boundp 'native-comp-deferred-compilation-deny-list)
    (defvar native-comp-deferred-compilation-deny-list '()))
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software\
/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; `autoload' is deprecated in Emacs 29.  Suppress that warning.
(with-suppressed-warnings ((obsolete autoload))
  (require 'straight))

;;;;;; Isolate package configuration with Use-package

(straight-use-package 'use-package)

;; On-demand loading of Use-package.
(if init-file-debug
    ;; Collect more information on debugging sessions.  This requires
    ;; Use-package be loaded explicitly.
    (progn
      ;; Compute statistics concerned use-package declarations.
      (setq-default use-package-compute-statistics t)
      ;; Report more details.
      (setq-default use-package-verbose t)
      ;; Report any load time.
      (setq-default use-package-minimum-reported-time 0)
      (require 'use-package))
  ;; On normal sessions (where I know my config works), make the expanded code
  ;; as minimal as possible.
  (setq-default use-package-expand-minimally t)
  ;; Use Use-package only for macro expansion.
  (eval-when-compile (require 'use-package)))

;;;;;; Custom keywords

(with-eval-after-load 'use-package-core
  ;; Functions provided by `use-package-core'.
  (declare-function use-package-list-insert "use-package")
  (declare-function use-package-normalize-symlist "use-package")
  (declare-function use-package-process-keywords "use-package")

  ;; Add keywords to the list.
  (dolist (keyword '(:defer-incrementally
                     :after-call))
    (push keyword use-package-deferring-keywords)
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :after)))

  ;; :defer-incrementally

  (defalias 'use-package-normalize/:defer-incrementally
    #'use-package-normalize-symlist)
  (defun use-package-handler/:defer-incrementally
      (name _keyword targets rest state)
    (use-package-concat
     `((zy-load-packages-incrementally
        ',(if (equal targets '(t))
              (list name)
            (append targets (list name)))))
     (use-package-process-keywords name rest state)))

  ;; :after-call

  (defalias 'use-package-normalize/:after-call
    #'use-package-normalize-symlist)
  (defun use-package-handler/:after-call
      (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (make-symbol (format "zy--after-call-%s-h" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (zy-log 'lazy "Lazy loading %s from %s" ',name ',fn)
                   (condition-case e
                       ;; If `default-directory' is a directory that doesn't
                       ;; exist or is unreadable, Emacs throws up file-missing
                       ;; errors, so we set it to a directory we know exists and
                       ;; is readable.
                       (let ((default-directory user-emacs-directory))
                         (require ',name))
                     ((debug error)
                      (message "Failed to load deferred package %s: %s" ',name e))))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                       `(add-hook ',hook #',fn)
                     `(advice-add #',hook :before #',fn))
                   forms)))
         (use-package-process-keywords name rest state))))))

;;;;;; Bind keys with General

(use-package general
  :straight t
  :demand t)
(declare-function general-def "general")
(declare-function general-unbind "general")

;;;;; Load the custom file

;; Save customizations outside the init file.
(setq! custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load the custom NOW.
(load custom-file 'noerror 'nomessage)

;;;;; ZyEmacs variables and keymaps

;; My personal information.  They are useful in various occasions, such as
;; snippet expansion.
(setq user-full-name "Eric Zhuo Chen"
      user-mail-address "zyxirchen@outlook.com")

(defgroup zyxir nil
  "Zyxir's customization layer over Emacs."
  :group 'emacs)

(defgroup zyxir-paths nil
  "Collection of paths of Zyxir's personal directories."
  :group 'zyxir)

(defun zy--set-zybox-path (sym path)
  "Set SYM to PATH, and set several other paths as well.

SYM is a symbol whose variable difinition stores the path of
Zybox, and PATH is the path of Zybox.  Once the location of Zybox
is determined, several other directories, like `org-directory',
`org-journal-directory', is decided by this function as well.

If PATH is nil, do not set any path."
  ;; Set the value of `sym' to `path'.
  (set sym path)
  ;; Set other directories only when `path' is a valid directory.
  (when (and (stringp path) (file-directory-p path))
    ;; The Org directory.
    (defvar org-directory)
    (setq! org-directory (expand-file-name "org" path))
    ;; My GTD directory and files.
    (defvar zy-gtd-dir)
    (setq! zy-gtd-dir org-directory
           zy-gtd-inbox-file
           (expand-file-name "inbox.org" zy-gtd-dir)
           zy-gtd-gtd-file
           (expand-file-name "gtd.org" zy-gtd-dir)
           zy-gtd-someday-file
           (expand-file-name "someday.org" zy-gtd-dir))
    ;; My other Org directory.
    (defvar zy-notes-file)
    (setq! zy-notes-file
           (expand-file-name "notes.org" org-directory))
    ;; My Org-journal directory.
    (setq! org-journal-dir
           (expand-file-name "org-journal" org-directory))
    ;; My Org-roam directory.
    (setq! org-roam-directory
           (expand-file-name "org-roam" org-directory))
    ;; My BibLaTeX databases.
    (defvar zy-bib-files)
    (defvar zy-ebib-bib-file)
    (let ((ebib-bib-file
           (expand-file-name "ebib/references.bib" path)))
      (unless (boundp 'zy-bib-files)
        (setq! zy-bib-files nil))
      (when (file-exists-p ebib-bib-file)
        (add-to-list 'zy-bib-files ebib-bib-file)
        (setq! zy-ebib-bib-file ebib-bib-file)))
    ;; Ebib paths.
    (setq! ebib-preload-bib-files zy-bib-files
           ebib-notes-directory
           (expand-file-name "ebib/notes" path)
           ebib-file-search-dirs
           `(,(expand-file-name "ebib/files" path)))
    ;; Citar paths. (Identical with Ebib ones)
    (setq! citar-bibliography zy-bib-files
           citar-library-paths
           `(,(expand-file-name "ebib/files" path)))))

(defcustom zy~zybox-dir nil
  "The Zybox directory, my personal file center."
  :group 'zyxir-paths
  :type '(choice (const :tag "Not set" nil) directory)
  :set #'zy--set-zybox-path)

(defvar zy-zyprojects-dir (expand-file-name "Zyprojects" "~")
  "The directory where I put Git repositories.")

(defvar zy-gtd-dir nil
  "Directory of my GTD (getting-things-done) files.
Automatically set when `zy~zybox-dir' is customized.")

(defvar zy-toggle-map (make-sparse-keymap)
  "Keymap for toggling various options.")
(fset 'zy-toggle-map zy-toggle-map)
(general-def "C-c t" 'zy-toggle-map)

(defvar zy-coding-map (make-sparse-keymap)
  "Keymap for coding-related actions.")
(fset 'zy-coding-map zy-coding-map)
(general-def "M-o" 'zy-coding-map)

(defun zy/test (&rest _)
  "Placeholder for code testing."
  (interactive)
  (message "No testing facility is implemented for this mode."))
(general-def :keymaps 'zy-coding-map
  "t" 'zy/test)

(defcustom zy~use-lsp-bridge nil
  "Non-nil means use Lsp-bridge instgead of Eglot.
If this is non-nil, Corfu will be turned off, too.

For Lsp-bridge to work, these additional Python packages have to
be installed: epc orjson sexpdata six paramiko."
  :group 'zyemacs
  :type 'boolean)

;;;;; Load Zyutils, the other part of the configuration

;; I keep a lot of function definitions and extra utilities in lisp/zyutils.el,
;; so that they can be autoloaded.

(use-package zyutils
  :load-path "lisp"
  :commands
  (;; Scratch buffer
   zy/scratch
   zy/scratch-elisp
   zy/scratch-org
   zy/scratch-python
   ;; Cursor movement
   zy/move-beginning-of-line
   ;; Line filling
   zy/unfill-paragraph
   ;; File operations
   zy/delete-file-and-buffer
   zy/rename-file-and-buffer
   zy/open-externally
   ;; Shell and Eshell
   zy/eshell-other-window
   zy/eshell-other-frame
   zy/shell-other-window
   zy/shell-other-frame
   ;; Org export to LaTeX
   zy/update-zylatex-file
   ;; Python
   zy/run-python-script)
  :general
  (:keymaps 'ctl-x-x-map
            "E" 'zy/open-externally))

;;;;; Start Emacs server

;; With the server on it is possible to edit files everywhere with only one Emacs
;; instance.

(use-package server
  :defer 1
  :config
  ;; Start the server if possible.
  (unless (and (fboundp 'server-running-p)
               (server-running-p))
    (server-start)))

;;;;; Terminal-specific settings

;; These settings only apply to Emacs running in a terminal.

(add-hook! tty-setup
  ;; Let Emacs handle mouse events by default.  However, normal xterm mouse
  ;; functionality is still available by holding Shift key.
  (xterm-mouse-mode 1))

;; Communicate with GUI clipboard within terminal Emacs.
;;
;; Caution: If Emacs is not built with GUI support (which is not my case), this package
;; have to use external tools to communicate with the clipboard, see URL
;; `https://elpa.gnu.org/packages/xclip.html'.
(use-package xclip
  :straight t
  :hook (emacs-startup . xclip-mode))

;;;;; Tweak garbage collection

;; This package enforces a sneaky garbage collection strategy to minimize GC
;; interference with user activity.

(use-package gcmh
  :straight t
  :defer 1
  :config
  (gcmh-mode 1))

;;;;; Keybinding management

;;;;;; Provide key-hints with Which-key

(use-package which-key
  :straight t
  ;; Which-key takes a lot of time (60 to 70 milliseconds) to load, and on most
  ;; occasions I won't need it on the first couple of keystrokes.
  :defer 2
  :config
  (which-key-mode 1)
  (setq!
   ;; If it turns out that I do need help from Which-key, show subsequent popups
   ;; right away.
   which-key-idle-secondary-delay 0.05))

;;;;;; Show unused keys via Free-keys

(use-package free-keys
  :straight t)

;;;;; Miscellaneous configuration

;; I put short, uncategorized configuration here.

(setq!
 ;; Suppress warning messages generated by ad-handle-definition.
 ad-redefinition-action 'accept
 ;; By default, Emacs asks the user if it should kill processes on exit.  VS Code does not
 ;; do that, and I find it annoying to do that, so just let Emacs kill processes
 ;; automatically.
 confirm-kill-processes nil
 ;; As an experienced Emacs user, I don't want any command disabled.
 disabled-command-function nil
 ;; A informational frame title.  Besides, my AutoHotkey scripts recognize my Emacs window
 ;; by the \"ZyEmacs\" prefix.
 frame-title-format '((if *wsl* "ZyEmacsWSL@" "ZyEmacs@")
                      (:eval (or
                              (file-remote-p default-directory 'host)
                              system-name))
                      " [%b]")
 ;; I usually use a Hybrid font like Sarasa Gothic, which contains tremendous amout of CJK
 ;; glyphs.  Disable compacting of font makes redisplay faster.
 inhibit-compacting-font-caches t
 ;; Do not report native compilation warnings and errors.  Those do not matter in most
 ;; occasions.
 native-comp-async-report-warnings-errors nil
 ;; The default (4 kB) is too low considering some language server responses are in 800 kB
 ;; to 3 MB.  So it is set to 1 MB as suggested by Lsp-mode.
 read-process-output-max (* 1024 1024)
 ;; Save existing clipboard text into kill ring before replacing it.  This
 ;; saves me so many time!
 save-interprogram-paste-before-kill t
 ;; Uniquify buffer names in a saner way.
 uniquify-buffer-name-style 'forward
 ;; Never use dialog boxes.
 use-dialog-box nil)

;;;; Searching, completion, and text-manipulation

;; This section contains bottom layer settings that control how Emacs works.

;;;;; Enhance completion with the Vertico suite

;;;;;; Orderless completion style

(use-package orderless
  :straight t
  :defer t
  :init
  ;; Enable the orderless completion style.
  (setq!
   ;; Use the orderless completion style.
   completion-styles '(orderless basic)
   completion-category-overrides '((file (styles . (partial-completion)))))
  :config
  ;; Custom Orderless dispatchers.  These are adapted from Protesilaus Stavrou's
  ;; configuration.

  ;; Regexp matching (default).
  (defun zy-orderless-regexp-dispatcher (pattern _index _total)
    "Regexp dispatcher using the percent sign."
    (when (string-suffix-p "%" pattern)
      `(orderless-regexp . ,(substring pattern 0 -1))))

  ;; Literal matching.
  (defun zy-orderless-literal-dispatcher (pattern _index _total)
    "Literal dispatcher using the equal sing suffix."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  ;; No-literal matching.
  (defun zy-orderless-no-literal-dispatcher (pattern _index _total)
    "Exclusive literal dispatcher using the exclamation suffix."
    (when (string-suffix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 0 -1))))

  ;; Prefixes matching.
  (defun zy-orderless-prefixes-dispatcher (pattern _index _total)
    "Prefixes dispatcher using the comma sign."
    (when (string-suffix-p "," pattern)
      `(orderless-prefixes . ,(substring pattern 0 -1))))

  ;; Apply matching styles and dispatchers.
  (setq!
   orderless-matching-styles '(orderless-literal
                               orderless-prefixes
                               orderless-flex
                               orderless-initialism
                               orderless-regexp)
   orderless-style-dispatchers '(zy-orderless-regexp-dispatcher
                                 zy-orderless-literal-dispatcher
                                 zy-orderless-no-literal-dispatcher
                                 zy-orderless-prefixes-dispatcher)))

;;;;;; Vertico and Marginalia (minibuffer completion)

(use-package vertico
  :straight t
  :hook zy-first-input
  :config
  (setq!
   ;; Make minibuffer intangible to cursor events.
   minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)
   ;; Enable recursive minibuffer.
   enable-recursive-minibuffers t)

  ;; Make minibuffer intangible.
  (add-hook! 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Indicator for completing-read-multiple.
  (defun crm-indicator (args)
    "Indicator for `completing-read-multiple'.

ARGS are the arguments passed."
    (defvar crm-separator)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Enable Vertico
  (vertico-mode 1))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode 1))

;;;;;; Consult (additional completing-read commands)

(use-package consult
  :straight t
  :commands (consult-completion-in-region
             consult-recent-file)
  :general
  ;; C-x map commands.
  (:keymaps 'ctl-x-map
            "b" 'consult-buffer)
  ;; Goto map commands (default prefix is M-g).
  (:keymaps 'goto-map
            "g" 'consult-goto-line
            "M-g" 'consult-goto-line
            "m" 'consult-mark
            "M" 'consult-global-mark
            "o" 'consult-outline
            "i" 'consult-imenu)
  ;; Search map commands (default prefix is M-s).
  (:keymaps 'search-map
            "g" 'consult-ripgrep
            "l" 'consult-line))


;;;;;; Embark (at-point dispatcher)

;; This technically is not completion, but it is related to the Vertico family.

(use-package embark
  :straight t
  :general
  ("M-m" 'embark-act))

;; Embark and Consult integration.
(use-package embark-consult
  :straight t
  :after (embark consult))

;;;;;; Corfu (at-point completion)

(use-package corfu
  :straight '(corfu :files (:defaults "extensions/*.el"))
  :unless zy~use-lsp-bridge
  :general (:keymaps 'corfu-map
                     ;; Do not interfere with global-mode keybindings except TAB.
                     "RET" nil
                     "<up>" nil
                     "<down>" nil
                     [remap previous-line] nil
                     [remap next-line] nil)
  :init
  (setq completion-cycle-threshold 3)
  (global-corfu-mode +1)
  :config
  (setq!
   ;; Enable auto completion.
   corfu-auto t
   ;; No delay for completion.
   corfu-auto-delay 0
   ;; I used to use 1, but it was too aggresive.  Now I think 3 (the default) is good.
   corfu-auto-prefix 3
   ;; Enable cycling.
   corfu-cycle t)

  ;; Disable auto completion in some modes.
  (setq-hook! (text-mode shell-mode eshell-mode)
    corfu-auto nil)

  ;; Insert additional RET in shell or eshell.  See URL
  ;; `https://github.com/minad/corfu#completing-in-the-eshell-or-shell'.
  (defun zy-corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode) (fboundp 'comint-send-input))
      (comint-send-input))))
  (advice-add #'corfu-insert :after #'zy-corfu-send-shell)

  ;; Corfu extensions.
  ;; Select candidates using Avy-style keys.
  (general-def :keymaps 'corfu-map
    "C-q" 'corfu-quick-insert)
  ;; Remember completion history.
  (corfu-history-mode +1)
  ;; Show candidate documentation in a popup.  Documentation is not shown automatically --
  ;; toggle it via "M-h" `corfu-info-documentation'.
  (setq corfu-popupinfo-delay '(nil . 0.0))
  (corfu-popupinfo-mode +1)

  ;; Enable Corfu in minibuffer.
  (defun zy-enable-corfu-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'zy-enable-corfu-in-minibuffer)

  ;; Fix on C-g with `corfu-mode' and `smartparens-mode'.
  ;;
  ;; When `corfu-mode' and `smartparens-mode' are both on, sometimes you have to hit C-g
  ;; twice to close the completion popup: the first hit calls
  ;; `sp-remove-active-pair-overlay', and the second calls `corfu-quit'.
  ;;
  ;; This is simple to solve with an advice.
  (with-eval-after-load 'smartparens
    (advice-add 'sp-remove-active-pair-overlay :after 'corfu-quit)))

;; Extra CAPFs (completion-at-point-function).
(use-package cape
  :unless zy~use-lsp-bridge
  :straight t
  :after corfu
  :config
  (setq-hook! (conf-mode prog-mode)
    completion-at-point-functions
    (append '(cape-symbol cape-keyword cape-file)
            completion-at-point-functions))
  (setq-hook! (text-mode)
    completion-at-point-functions
    (append '(cape-symbol cape-file)
            completion-at-point-functions)))

;; Corfu in Terminal.
(use-package corfu-terminal
  :straight (corfu-terminal :host codeberg :repo "akib/emacs-corfu-terminal")
  :unless (display-graphic-p)
  :hook (corfu-mode))

;;;;; Tweak cursor movement

;; Use my own `zy/move-beginning-of-line'.
(use-package zy-curmov
  :defer t
  :general
  ;; Let "C-a" move point between the indentation and real line beginning.  Does
  ;; not work when `visual-line-mode' is on (C-a is remapped in
  ;; `visual-line-mode' anyway).
  ([remap move-beginning-of-line] 'zy/move-beginning-of-line))

;;;;;; Subword

;; Enable subword movement (movement in camel case compartments) in some modes.
(use-package subword
  :hook (eshell-mode conf-mode prog-mode shell-mode minibuffer-mode))

;;;;;; Avy (quick text jump)

(use-package avy
  :straight t
  :general
  ("M-z" 'avy-goto-char
   "M-Z" 'avy-goto-char-2))

;;;;; Tweak indentation

(use-package zy-indentation
  :defer t
  :init
  (setq!
   ;; Always use spaces.
   indent-tabs-mode nil))

;; Mode used to highlight indentation.
(use-package highlight-indent-guides
  :straight t
  :general
  (:keymaps 'zy-toggle-map
            "i" 'highlight-indent-guides-mode)
  :config
  (setq!
   ;; Use filling to indicate indentation.
   highlight-indent-guides-method 'fill
   ;; Highlight the current indentation.
   highlight-indent-guides-responsive 'top))

;;;;; Tweak selection

(use-package zy-selection
  :defer t
  :init
  (delete-selection-mode 1))

;;;;; Search in buffer (Isearch)

(use-package isearch
  :config
  (setq!
   ;; Show match number in the search prompt.
   isearch-lazy-count t
   ;; Let one space match a sequence of whitespace chars.
   isearch-regexp-lax-whitespace t
   ;; Remember more regexp searches.
   regexp-search-ring-max 200
   ;; Remember more normal searches.
   search-ring-max 200))

;; Isearch-mb is an enhanced version of Isearch.

(use-package isearch-mb
  :straight t
  :after-call (isearch-forward isearch-backward)
  :config
  (isearch-mb-mode t))

;;;;; Line filling

;; This controls how texts (paragraphs or comments) should be wrapped to a given
;; column count.
(use-package line-filling
  :defer t
  :general
  ("M-Q" 'zy/unfill-paragraph)
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :init
  (setq!
   ;; `fill-column' controls the line length when using commands like `fill-paragraph'.
   ;; Google suggests a line length limit of 80 characters, which is a sane default.
   ;; However, this should not be mandatory: feel free to exceed the limit at any time,
   ;; unless when paid to write lines of code shorter than 80 characters.
   fill-column 80
   ;; These settings are adapted from Protesilaus Stavrou's configuration.
   sentence-end-double-space t
   sentence-end-without-period nil
   colon-double-space nil
   use-hard-newlines nil
   adaptive-fill-mode t))

;;;;; Whitespaces

(use-package whitespace
  :general
  (;; Remap "M-SPC" (`just-one-space') to `cycle-spacing'.  This is the default
   ;; in Emacs 29.1.
   "M-SPC" 'cycle-spacing
   ;; Sometimes "M-SPC" is unavailable (occupied by the window manager, like in WSLg), so
   ;; use this as an alternative.  "M-\\" is bound to `delete-horizontal-space' by
   ;; default, which is very similiar to cycle-spacing.
   "M-\\" 'cycle-spacing)
  (:keymaps 'zy-toggle-map
            ;; Toggle whitespace visualization.
            "SPC" 'whitespace-mode)
  :init
  ;; Show trailing whitespace for all kinds of files.
  (add-hook! (prog-mode text-mode conf-mode)
    (setq-local show-trailing-whitespace t)))

;;;;; Manipulate word cases

(use-package zy-cases
  :defer t
  :init
  (general-def
    [remap upcase-word] 'upcase-dwim
    [remap downcase-word] 'downcase-dwim
    [remap capitalize-word] 'capitalize-dwim))

;;;;; Word wrapping and visual lines

;; Word wrapping is often enabled by `visual-line-mode', so we defer loading of
;; these configurations until it being run.

(add-transient-hook! `visual-line-mode
  (setq!
   ;; Allow line breaking after CJK characters.  Setting this with `setq!' will
   ;; load kinsoku.el automatically, which enhances line breaking.
   word-wrap-by-category t))

;; Enable the mode for all text modes.

(add-hook 'text-mode-hook 'visual-line-mode)

;;;;; Yasnippet (snippet support)

(use-package yasnippet
  :straight t
  :init
  ;; Enable yasnippet globally on first buffer visit.
  (add-hook! zy-first-buffer (yas-global-mode 1))
  :general
  ;; I dislike expanding snippets with TAB, which I use for reindenting a lot.
  ;; So I just remap `dabbrev-expand' (M-/ by default) for this.
  (:keymaps 'yas-minor-mode-map
            "<tab>" nil
            "TAB" nil
            [remap dabbrev-expand] 'yas-expand)
  :init
  ;; My own snippets.
  (setq! yas-snippet-dirs (list
                           (expand-file-name "etc/snippets"
                                             user-emacs-directory))))

;; Predefined snippets.
(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

;; Insert snippets via Consult-yasnippet, with fancy live preview.
(use-package consult-yasnippet
  :straight t
  :general
  ("C-c s" 'consult-yasnippet
   "C-c S" 'consult-yasnippet-visit-snippet-file))

;;;;; Smartparens (parenthesis automation)

(use-package smartparens
  :straight '(smartparens :host github :repo "Fuco1/smartparens"
                          :fork (:repo "zyxir/smartparens" :branch "dev" :protocol ssh))
  :defer t
  :hook (conf-mode prog-mode text-mode comint-mode eshell-mode minibuffer-mode)
  :general
  ;; Failed to get used to the default keybindings (which is actually Fuco1's
  ;; keybindings), I ended up making my own keybindings.  My own keybindings have two
  ;; advantages:
  ;;
  ;; First, keys are binded with `general-def', so that they can be overviewebbbd with
  ;; `general-describe-keybindings'.
  ;;
  ;; Second, unlike Fuco1's config, my keybindings trys to preserve Emacs behavior, so no
  ;; confusion is created.
  ;;
  ;; However, my keybindings indeed provide less feature than Fuco1's, so that my small
  ;; brain can remember them, and bind them to my musle memory.
  (:keymaps 'smartparens-mode-map
            ;;
            ;; --- Moving and marking ---
            ;;
            ;; Was `mark-sexp'.
            "C-M-SPC" 'sp-mark-sexp
            ;; Was `forward-sexp'.
            "C-M-f" 'sp-forward-sexp
            ;; Was `backward-sexp'.
            "C-M-b" 'sp-backward-sexp
            ;; Was an alias of `backward-sentence'.
            "M-A" 'sp-begining-of-sexp
            ;; Was an alias of `forward-sentence'.
            "M-E" 'sp-end-of-sexp
            ;; Was `down-list'.
            "C-M-d" 'sp-down-sexp
            ;; Was `backward-up-list'.
            "C-M-u" 'sp-backward-up-sexp
            ;; Was `backward-list'.
            "C-M-p" 'sp-previous-sexp
            ;; Was `forward-list'.
            "C-M-n" 'sp-next-sexp
            ;;
            ;; --- Manipulation ---
            ;;
            ;; Was `kill-sexp'.
            "C-M-k" 'sp-kill-sexp
            ;; Was `backward-kill-sexp', inaccessible in terminal.
            "C-M-<backspace>" 'sp-backward-kill-sexp
            ;; Also was `backward-kill-sexp', accessible in terminal.
            "C-M-<delete>" 'sp-backward-kill-sexp
            ;; Was `insert-parentheses', which is useless as long as Smartparens is on.
            "M-(" 'sp-backward-unwrap-sexp
            ;; Was `move-past-close-and-reindent'.
            "M-)" 'sp-unwrap-sexp
            ;; Was `transpose-sexp'.
            "C-M-t" 'sp-transpose-sexp
            ;;
            ;; --- Useful commands ---
            ;;
            ;; Was `indent-region', which can be achieved via <tab>.
            "C-M-\\" 'sp-indent-defun
            ;; Was nothing.
            "M-\"" 'sp-rewrap-sexp)
  ;; Extra Smartparens commands with the "M-]" prefix.
  (:keymaps 'smartparens-mode-map
            :prefix "M-]"
            "i" 'sp-change-inner
            "s" 'sp-split-sexp
            "j" 'sp-join-sexp
            "ka" 'sp-splice-sexp-killing-around
            "kf" 'sp-splice-sexp-killing-forward
            "kb" 'sp-splice-sexp-killing-backward
            "fb" 'sp-forward-barf-sexp
            "bb" 'sp-backward-barf-sexp
            "fs" 'sp-forward-slurp-sexp
            "bs" 'sp-backward-slurp-sexp)
  :config
  (setq
   ;; Do not show overlays.
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   ;; Do not auto insert colon for Python.
   sp-python-insert-colon-in-function-definitions nil)

  ;; Apply default config.
  (require 'smartparens-config)

  ;; Turn on strict mode for Lisp buffers.
  (add-hook! (lisp-data-mode) (smartparens-strict-mode 1)))

;;;;; Mouse yank

;; Disable yanking with the middle mouse button.

(general-unbind "<mouse-2>" "<down-mouse-2>")

;;;;; Keyboard macros

(use-package kmacro
  :defer t
  :config
  (with-eval-after-load 'corfu
    (defadvice! zy--disable-corfu-while-kmacro (fn &rest rest)
      "Disable Corfu while executing a keyboard macro."
      :around 'kmacro-call-macro
      (defvar corfu-mode)
      (declare-function corfu-mode "corfu")
      (let ((corfu-enabled-p corfu-mode))
        (when corfu-enabled-p (corfu-mode -1))
        (apply fn rest)
        (when corfu-enabled-p (corfu-mode 1))))))

;;;;; Iteractive align with Ialign

(use-package ialign
  :straight t
  :general ("C-x l" 'ialign))

;;;;; Rg -- Ripgrep frontend

(use-package rg
  :straight t
  :commands rg rg-menu rg-dwim
  :general
  (:keymaps 'isearch-mode-map
            "M-s r" 'rg-isearch-menu))

;;;;; Wgrep -- writable grep buffer

(use-package wgrep
  :straight t
  :defer t)

;;;;; Tree-sitter integration

(use-package treesit-auto
  :straight t
  :demand t
  :config
  ;; Install language support manually.
  (setq treesit-auto-langs '(c python))
  (global-treesit-auto-mode))

;;;; File, buffer, window, project management

;;;;; No auto save or backup files

(use-package files
  :init
  ;; Make no auto save or backup files.
  (setq! auto-save-default nil
         make-backup-files nil))

;;;;; Configure encoding and locale

(use-package zy-encoding
  :defer t
  :init
  ;; Set everything to UTF-8.
  (set-language-environment "UTF-8")
  ;; Use "C" for English locale.
  (setq! system-time-locale "C")

  ;; Handy macro to set process coding system.
  (defmacro zy-set-process-coding-system! (&rest args)
    "Set coding system for processes.
ARGS is (PROC VAL PROC VAL ...).

If PROC is t, set `default-process-coding-system' to VAL.

If PROC is a process name in lowercase, convert it to a regexp
that match the process in any case, like \"cmd\" is converted to
\"[cC][mM][dD]\", then set its coding system to VAL.

VAL is the same as in `process-coding-system-alist'."
    (declare (indent defun))
    (let ((result '(progn))
          proc val)
      (push '(setq process-coding-system-alist nil) result)
      (while (and (setq proc (pop args))
                  (setq val (pop args)))
        (if (eq proc t)
            (push (list 'setq 'default-process-coding-system val)
                  result)
          (let ((proc-regexp (apply 'concat
                                    (mapcar (lambda (char)
                                              (concat "["
                                                      (list
                                                       (downcase char)
                                                       (upcase char))
                                                      "]"))
                                            (string-to-list proc)))))
            (push (list 'add-to-list (quote 'process-coding-system-alist)
                        (list 'cons proc-regexp val))
                  result))))
      (reverse result)))
  (declare-function zy-set-process-coding-system! "init.el")

  ;; On Microsoft Windows, encoding is painful to deal with.  Emacs's default locale
  ;; settings work sometimes, but not for every program.
  ;;
  ;; The bad thing is, some program use both GBK for input and output, like cmdproxy; some
  ;; program use UTF-8 for input and GBK for output, like rg.  Some Emacs packages (like
  ;; rg.el) call program through cmd, which add an extra layer of encoding, making
  ;; encoding harder to detect and set.
  ;;
  ;; I read a lot of configuration online and came up with this.  The following
  ;; configuration works with most external processes, but not with shell commands because
  ;; they add another layer of complexity, as they run commands through cmd.exe.
  (when (and
         ;; The operating system is Microsoft Windows.
         (eq system-type 'windows-nt)
         ;; Code page 936, the character encoding for simplified Chinese.
         (eq locale-coding-system 'cp936))
    ;; (set-language-environment "Chinese-GBK")
    (zy-set-process-coding-system!
      "cmdproxy" 'gbk-dos
      "plink" 'gbk-dos
      "python" 'gbk-dos
      t '(utf-8-dos . gbk-dos))))

;;;;; Additional scratch buffer

;; I have tweaked scratch buffer to be in `fundamental-mode' in the "Reduce GUI
;; noises" section.  These keys make it easier to quickly open a scratch buffer
;; for temporary text or Emacs Lisp evaluation.

(general-def :keymaps 'ctl-x-x-map
  "k" 'zy/scratch
  "l" 'zy/scratch-elisp
  "o" 'zy/scratch-org
  "p" 'zy/scratch-python)

;; This section enhances the basic text-editing capability of Emacs.

;;;;; Structure buffers with Outline-minor-mode

;; Hook `outline-minor-mode' to specific language modes.

(use-package outline
  :general
  (:keymaps 'zy-toggle-map
            "o" 'outline-minor-mode)
  :config
  (setq!
   ;; Cycle outline visibility with TAB.
   outline-minor-mode-cycle t
   ;; Highlight outline headings.
   outline-minor-mode-highlight 'override))

;;;;; Automatically revert file-visiting buffers

(use-package autorevert
  :hook (zy-first-file . global-auto-revert-mode)
  :config
  ;; Do not auto revert some modes.
  (setq! global-auto-revert-ignore-modes '(pdf-view-mode)))

;;;;; Persist variables across sessions with Savehist

;; This is adopted from Doom Emacs.
(use-package savehist
  ;; persist variables across sessions
  :init (savehist-mode 1)
  :config
  (setq! savehist-save-minibuffer-history t
         savehist-autosave-interval nil     ; save on kill only
         savehist-additional-variables
         '(kill-ring                        ; persist kill ring
           register-alist                   ; persist macros
           mark-ring global-mark-ring       ; persist marks
           search-ring regexp-search-ring)) ; persist searches

  (add-hook! 'savehist-save-hook
    (defun zy-savehist-unpropertize-variables-h ()
      "Remove text properties from `kill-ring'.

This reduces savehist cache size."
      (setq kill-ring
            (mapcar #'substring-no-properties
                    (cl-remove-if-not #'stringp kill-ring))
            register-alist
            (cl-loop for (reg . item) in register-alist
                     if (stringp item)
                     collect (cons reg (substring-no-properties item))
                     else collect (cons reg item))))

    (declare-function savehist-printable "savehist")
    (defun zy-savehist-remove-unprintable-registers-h ()
      "Remove unwriteable registers (e.g. containing window configurations).

Otherwise, `savehist' would discard `register-alist' entirely if
we don't omit the unwritable tidbits."
      ;; Save new value in the temp buffer savehist is running
      ;; `savehist-save-hook' in. We don't want to actually remove the
      ;; unserializable registers in the current session!
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))))

;;;;; Persist locations in buffer with Saveplace

(use-package saveplace
  :hook (zy-first-file . save-place-mode)
  :config
  (defadvice! zy--dont-prettify-saveplace-cache-a (fn)
    "`save-place-alist-to-file' uses `pp' to prettify the contents of its cache.
`pp' can be expensive for longer lists, and there's no reason to
prettify cache files, so this replace calls to `pp' with the much
faster `prin1'."
    :around 'save-place-alist-to-file
    (cl-flet ((pp #'prin1)) (funcall fn))))

;;;;; Record recently opened files with Recentf

(use-package recentf
  :defer-incrementally easymenu tree-widget timer
  :general
  (:keymaps 'ctl-x-map
            "C-r" 'recentf)
  :config
  (setq!
   ;; Do not do auto cleanups except its a daemon session.
   recentf-auto-cleanup (if (daemonp) 300)
   ;; Default is 20, which is far from enough.
   recentf-max-saved-items 200)
  ;; Clean up recent files when quitting Emacs.
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

;;;;; Version control (VC, Magit, Diff-hl, and Git-timemachine)

;; I seldom use the built-in VC, but it affects Emacs in various circumstances, so its
;; behavior has to be tweaked.
(use-package vc
  :defer t
  :init
  (setq!
   ;; Do not ask me if visiting a symbolic link to a file under version control.  Just
   ;; visit it, and tell me about it in the echo area.
   vc-follow-symlinks t))

;; Use Magit as the Git interface.
(use-package magit
  :straight t
  :defer-incrementally
  (dash f s with-editor git-commit package eieio transient)
  :general
  (:keymaps 'ctl-x-map
            "g" 'magit-status
            "G" 'magit-dispatch
            "M-g" 'magit-file-dispatch)
  :init
  (setq!
   ;; Do not use default Magit key bindings.
   magit-define-global-key-bindings nil
   ;; Show commit time in the status buffer.
   magit-status-margin '(t age magit-log-margin-width nil 18)))

;; Extra autoloaded magit commands.
(use-package magit-extras
  :commands
  (magit-project-status)
  :config
  (require 'project))

;; Use Diff-hl to highlight file changes.
(use-package diff-hl
  :straight t
  :hook (zy-first-file . global-diff-hl-mode)
  :init
  ;; I set a key binding here because I don't want its default key binding
  ;; overriding my Magit keys.  I never use this though.
  (setq! diff-hl-command-prefix (kbd "C-c M-d"))
  :config
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package git-timemachine
  :straight t
  :commands git-timemachine)

;;;;; Manage files with Dired

;; Dired is a handy file manager built-in to Emacs.

(use-package dired
  :defer t
  :general
  (:keymaps 'dired-mode-map
            "E" 'zy/open-externally)
  :config
  (setq!
   ;; Move to trash when available.
   delete-by-moving-to-trash t
   ;; Revert Dired buffers if the directory has changed.
   dired-auto-revert-buffer 'dired-directory-changed-p
   ;; Guess the target directory.
   dired-dwim-target t
   ;; Command line switches used for ls.
   dired-listing-switches (eval-when-compile
                            (string-join
                             '(;; No "." and "..".
                               "--almost-all"
                               ;; No group names.
                               "--no-group"
                               ;; Append indicator to entries.
                               "--classify"
                               ;; Natural sort of version numbers.
                               "-v"
                               ;; Group directories and show them first.
                               "--group-directories-first"
                               ;; Show human readable sizes.
                               "--human-readable"
                               ;; Show ISO 8601 timestamps.
                               "--time-style=long-iso"
                               ;; Must be included for dired.
                               "-l")
                             " "))
   ;; Make directories at the title bar clickable.
   dired-make-directory-clickable t
   ;; Allow mouse to drag files.
   dired-mouse-drag-files t
   ;; Do not ask for recursive operations, just like any other modern file
   ;; manager will do.
   dired-recursive-copies 'always
   dired-recursive-deletes 'always)

  ;; Hooks
  (add-hook! dired-mode
    ;; Hide details like size, modification time, owner, ... by default.
    'dired-hide-details-mode))

;; Built-in auxilary functionalities for Dired.
(use-package dired-aux
  :defer t
  :after dired
  :config
  (setq!
   ;; Revert directory if it is not remote.
   dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))
   ;; Better match filenames with Isearch.
   dired-isearch-filenames 'dwim
   ;; Ask me about directory creation.
   dired-create-destination-dirs 'ask
   ;; Rename file via version control program.
   dired-vc-rename-file t))

;; Tree style view for Dired.
(use-package dired-subtree
  :straight t
  :defer t
  :after dired
  :general
  (:keymaps 'dired-mode-map
            "TAB" 'dired-subtree-toggle
            [tab] 'dired-subtree-toggle
            "S-TAB" 'dired-subtree-remove
            [backtab] 'dired-subtree-remove))

;; Editable Dired buffer.
(use-package wdired
  :defer t
  :config
  (setq!
   ;; Allow to modify file permisions.
   wdired-allow-to-change-permissions t
   ;; Create parent directories smartly.
   wdired-create-parent-directories t))

;;;;; Manage project with Projectile

;; I decide to switch to Projectile from the built-in Project.el.  Projectile is much more
;; feature rich by now (2023-04-05). By far, these features are provided by Projectile,
;; and not provided by Project.el:
;;
;; 1. Recognizing a project smartly (with all kinds of markers like ".projectile",
;; "build.sbt" or "Makefile").  In Project I have to manually configure this.
;;
;; 2. Better integration with Ripgrep, Fd, Vterm and many things.  Better integration with
;; all kinds of VC system.
;;
;; 3. `projectile-find-file' and other commands put recent file at front.
;;
;; 4. Many extremely useful commands like `projectile-recentf' and `projectile-ibuffer'.
;;
;; In conclusion, Projectile provides too many killing feature that a modern code editor
;; should provide.  Project is just not as useful as it.  Though I am currently bound to
;; Project.el by my musle memory, I have to switch to Projectile.
;;
;; The only downside of Projectile is that it doesn't provide a Consult-style conpletion
;; UI, which Project.el does by default.  But that is not very important, though.

(use-package projectile
  :straight t
  :commands (projectile-add-known-project
             projectile-discover-projects-in-directory
             projectile-discover-projects-in-search-path
             projectile-reset-known-projects)
  :hook zy-first-file
  :defer 2
  :general
  ;; Disable shortcuts from the built-in Project.el. I'm so used to it that I have to
  ;; break my musle memory.
  (:keymaps 'global-map
            "C-x 4 p" nil
            "C-x 5 p" nil
            "C-x t p" nil
            "C-x p" nil)
  ;; Don't load Projectile at startup.  Load Projectile on theses commands.
  (:keymaps 'global-map
            "C-x p p" 'projectile-switch-project
            "C-x p f" 'projectile-find-file
            "C-x p F" 'projectile-find-file-in-known-projects)
  (:keymaps 'projectile-mode-map
            "C-x p" 'projectile-command-map)
  :config
  (projectile-mode 1)
  (setq!
   ;; Discover projects in these directories.
   projectile-project-search-path (append
                                   ;; My Zyprojects directory.
                                   (if (and (stringp zy-zyprojects-dir)
                                            (file-directory-p zy-zyprojects-dir))
                                       (list zy-zyprojects-dir)
                                     nil)
                                   ;; The Emacs configuration directory.
                                   (list user-emacs-directory))
   ;; Ask for command after a project switch, like the built-in Project.el.
   projectile-switch-project-action 'projectile-commander))

;; Automatically run "git fetch" on Projectile projects.
(use-package projectile-git-autofetch
  :straight t
  :when (and *posix* (not *wsl*))
  :hook projectile-mode)

;;;;; Additional file operations

;; Additional file operations.

(general-def :keymaps 'ctl-x-x-map
  "d" 'zy/delete-file-and-buffer
  "R" 'zy/rename-file-and-buffer)

;;;;; Configure the built-in tab bar

;; A tab in Emacs is not like a tab in a browser or VS Code.  It is more like a
;; workspace.

(use-package tab-bar
  :defer t
  :config
  ;; Customizing many of these options triggers the load of `tab-bar-mode', so
  ;; just set them instead.
  (setq
   ;; Do not auto determine tab widths.
   tab-bar-auto-width nil
   ;; Show the tab bar, but not the close button.
   tab-bar-show t
   tab-bar-close-button-show nil
   ;; Reduce tab bar UI elements.
   tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

  ;; Tab name with more paddings.
  (defun zy--tab-bar-tab-name-format (tab &optional _i)
    "Fucntion to produce centered tab name."
    (propertize
     (concat "  " (alist-get 'name tab) "  ")
     'face (funcall tab-bar-tab-face-function tab)))
  (setq! tab-bar-tab-name-format-function 'zy--tab-bar-tab-name-format)

  ;; Disable C-<tab> for tab switching, as it collides with other packages, like
  ;; Magit.
  (general-unbind "C-<tab>"))

;;;;; Configure scrolling

;; Tweaked scrolling experience.

(use-package zy-scrolling
  :defer t
  :init
  ;; Enable smooth scroll on GTK.
  (when (memq window-system '(x pgtk))
    (pixel-scroll-precision-mode 1))

  (defun zy--golden-ratio-scroll-a (fn &rest arg)
    "Make scroll commands scroll 0.618 of the screen.

This is an :around advice, and FN is the adviced function."
    (dlet ((next-screen-context-lines
            (round (* 0.382 (window-height)))))
      (apply fn arg)))
  (advice-add #'scroll-up-command :around 'zy--golden-ratio-scroll-a)
  (advice-add #'scroll-down-command :around 'zy--golden-ratio-scroll-a))

;;;;; Sudo edit

;; Allow to switch editing rights on an already opened read-only file.

(use-package sudo-edit
  :straight t
  :commands 'sudo-edit)

;;;; User interface

;; This sections concentrates on improving the user interface of GNU Emacs,
;; either graphical or terminal.

;;;;; Configure themes

(defcustom zy~default-theme 'doom-one
  "Default theme of Emacs.
Will only take effect after restart.  If you want to tweak the
theme, use `customize-themes' instead."
  :group 'zyxir
  :type 'symbol)

(use-package doom-themes
  :straight t
  :demand t
  :config
  (setq! doom-themes-enable-bold t
         doom-themes-enable-italic t)
  (load-theme zy~default-theme 'no-confirm))

(use-package solaire-mode
  :straight t
  :after doom-themes
  :hook (zy-first-buffer . solaire-global-mode))

;;;;; Configure fonts

;; Font setter for other character sets

(defvar zy--fontset-cnt 0
  "Number of fontsets generated by `zy-set-face-charset-font'.")

(defconst zy-cjk-charsets '(han cjk-misc bopomofo kana hangul)
  "CJK character sets.")

;; Font faces setup

(defcustom zy~font-size 18
  "The pixel size of font in `default' face."
  :type 'integer
  :group 'zyemacs
  :set #'(lambda (sym size)
           (set sym size)
           (when (fboundp 'zy/setup-font-faces)
             (zy/setup-font-faces))))

(defun zy-set-face-charset-font (face frame charset font)
  "Set the font used for character set CHARSET in face FACE.

This function has no effect if `display-graphic-p' returns nil,
since fontset is not supported in console mode.

FRAME specifies the frame to set in.  When FRAME is nil or
omitted, set it for all existing frames, as well as the default
for new frames.

CHARSET specifies the character set to set font for.  CHARSET
could also be a list of character sets, where every character set
will be set for.

FONT is the font to be set.  It can be a `font-spec' object, or a
font name string.

This is a convenient method to set font for specific character
set (like CJK characters or symbols).  However, the fontset
system of Emacs is complicated, and not very straightforward.
Instead of playing with `font-spec', fontsets and frame
attributes, this function provides a simpler interface that just
does the job."
  (when (display-graphic-p)
    (let* (;; The fontset that we are going to manipulate
           (fontset (face-attribute face :fontset frame))
           ;; If the fontset is not specified
           (unspecified-p (equal fontset 'unspecified)))
      ;; If the fontset is not specified, create a new one with a
      ;; programmatically generated name
      (when unspecified-p
        (setq fontset
              (new-fontset
               (format "-*-*-*-*-*--*-*-*-*-*-*-fontset-zy%d"
                       zy--fontset-cnt)
               nil)
              zy--fontset-cnt (+ 1 zy--fontset-cnt)))
      ;; Set font for the fontset
      (if (listp charset)
          (mapc (lambda (c)
                  (set-fontset-font fontset c font frame 'prepend))
                charset)
        (set-fontset-font fontset charset font frame))
      ;; Assign the fontset to the face if necessary
      (when unspecified-p
        (set-face-attribute face frame :fontset fontset)))))

;; I used to write very flexible font configuration codes that defines a tons of
;; faces and automatically picks the first available font from a list, but that
;; turned out to be too complicated and heavy.  Now I just hard-coded the font
;; names and rely on the default font fallback mechanism.

;; Anyway this is just my personal configuration, I can change the code at any
;; time.

(defun zy/setup-font-faces ()
  "Setup font for several faces.

This function does not work correctly on Terminal Emacs."
  (interactive)
  (defvar zy~font-size)
  ;; Default face.
  (set-face-attribute 'default nil
                      :font (font-spec :family "Sarasa Mono HC"
                                       :size zy~font-size))
  (zy-set-face-charset-font 'default nil zy-cjk-charsets "Sarasa Mono HC")
  ;; Native emoji and symbol fonts.
  (let ((symbol-and-emoji-fonts
         (pcase system-type
           ('windows-nt '("Segoe UI Symbol" "Segoe UI Emoji"))
           ('gnu/linux '("Noto Sans Symbol" "Noto Color Emoji")))))
    (zy-set-face-charset-font 'default nil 'symbol (car symbol-and-emoji-fonts))
    (zy-set-face-charset-font 'default nil 'emoji (cadr symbol-and-emoji-fonts)))
  ;; Fixed-pitch face.
  (set-face-attribute 'fixed-pitch nil :font "Sarasa Mono HC"
                      :height 'unspecified)
  (zy-set-face-charset-font 'fixed-pitch nil
                            zy-cjk-charsets "Sarasa Mono HC")
  ;; Variable-pitch face.
  (set-face-attribute 'variable-pitch nil :font "Roboto")
  (zy-set-face-charset-font 'variable-pitch nil
                            zy-cjk-charsets "Sarasa Mono HC"))

(defun zy-maybe-setup-font-faces (&rest _)
  "Try to setup font faces.

If GUI is not available currently, add itself to
`after-make-frame-functions', so that it can be run again the
next time a frame is created.

If GUI is available, setup font with `zy/setup-font-faces', and
remove itself from `after-make-frame-functions' if it is there.
Return what `zy/setup-font-faces' returns."
  (if (display-graphic-p)
      (prog1
          (condition-case e
              (zy/setup-font-faces)
            (error (lwarn 'font :warning "%s: %s" (car e) (cdr e))))
        (remove-hook! '(after-make-frame-functions
                        server-after-make-frame-hook)
          #'zy-maybe-setup-font-faces))
    (add-hook! '(after-make-frame-functions
                 server-after-make-frame-hook)
      #'zy-maybe-setup-font-faces)))

(use-package zy-font
  :defer t
  :init
  (zy-maybe-setup-font-faces))

;;;;; Configure the mode line

;; Use the fantastic Doom modeline.
(use-package doom-modeline
  :straight t
  :demand t
  :config
  ;; Tweak and enable the Doom modeline.
  (setq doom-modeline-enable-word-count t
        doom-modeline-display-misc-in-all-mode-lines nil
        doom-modeline-icon (null *windows*))
  (doom-modeline-mode 1)
  ;; Additionally, enable column number mode.
  (column-number-mode 1))

;; Show time.  This helps on fullscreen work sessions.
(use-package time
  :straight t
  :demand t
  :config
  (setq!
   ;; Use 24 hour format.
   display-time-24hr-format t)
  (display-time-mode 1))

;;;;; Configure in-buffer display

;;;;;; Configure heading display

;; Customize the appearance of headings in Org-mode, Markdown-mode, and
;; Outline-mode.

(defun zy--setup-heading-appearance ()
  "Setup heading appearance.
This tweaks the heights and fonts of headings.

Should be run again after theme switch."
  (let ((font "Roboto Slab")
        face num)
    ;; Setup for `outline-mode' and `outline-minor-mode'.
    (with-eval-after-load 'outline
      (setq num 1)
      (while (<= num 8)
        (setq face (intern (format "outline-%d" num)))
        (set-face-attribute face nil :height
                            (max (- 1.5 (* 0.1 num)) 1.1))
        (set-face-attribute face nil :family font)
        (setq num (1+ num))))
    ;; Setup for `markdown-mode'.
    (with-eval-after-load 'markdown-mode
      (setq num 1)
      (while (<= num 6)
        (setq face (intern (format "markdown-header-face-%d" num)))
        (set-face-attribute face nil :height
                            (max (- 1.5 (* 0.1 num)) 1.1))
        (set-face-attribute face nil :family font)
        (setq num (1+ num))))
    ;; Setup for `org-mode'.  Doom themes make Org heading faces inherit Outline
    ;; faces, so only set document title for them.
    (with-eval-after-load 'org
      (set-face-attribute 'org-document-title nil :height 1.4)
      (set-face-attribute 'org-document-title nil :family font))))

;; Set them now, and after each theme switch.
(zy--setup-heading-appearance)
(add-hook 'zy-load-theme-hook #'zy--setup-heading-appearance)

;;;;;; Highlight TODO and similiar keywords

(use-package hl-todo
  :straight t
  :hook (prog-mode text-mode conf-mode)
  :general
  (:keymaps 'hl-todo-mode-map
            "M-P" 'hl-todo-previous
            "M-N" 'hl-todo-next)
  :init
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF"))))

;;;;;; Visually align tables with Valign

;; Valign provide pixel-perfect alignment for tables.

(use-package valign
  :straight t
  :hook (org-mode markdown-mode))

;;;;; Useful UI elements

;;;;;; Highlight current line with Hl-line-mode

(use-package hl-line
  :hook (prog-mode text-mode conf-mode org-agenda-mode)
  :config
  ;; Only highlight line in the current window.
  (setq! hl-line-sticky-flag nil))

;;;;;; Show line numbers

;; Line numbers display.
(use-package display-line-numbers
  :defer t
  :general
  (:keymaps 'zy-toggle-map
            "l" 'display-line-numbers-mode)
  :init
  (add-hook! (prog-mode text-mode conf-mode)
    ;; The argument "1" is important, otherwise in some rare circumstances the hook will
    ;; trigger twice and turn off the mode.
    (display-line-numbers-mode 1))

  ;; Explicitly define a width to reduce the cost of on-the-fly computation.  4 is a good
  ;; default, as programs seldom exceed 10000 lines.
  (setq-default display-line-numbers-width 4)

  ;; Show absolute line numbers for narrowed regions to make it easier to tell
  ;; the buffer is narrowed, and where you are, exactly.
  (setq-default display-line-numbers-widen t))

;;;;;; Rainbow colored delimiters

;; Hook `rainbow-delimiters-mode' to specific language modes.

(use-package rainbow-delimiters
  :straight t
  :hook prog-mode)

;;;;;; Never lose the cursor with Pulsar

;; On Emacs version 28 or earlier, double-buffering is not supported on
;; Windows, and pulsar will cause serious flickering.  So disable Pulsar if
;; that is the case.
(unless (eval-when-compile
          (and (memq system-type '(windows-nt cygwin))
               (< emacs-major-version 29)))
  (use-package pulsar
    :straight t
    :hook (zy-first-buffer . pulsar-global-mode)
    :config
    ;; More pulse functions.
    (when (boundp 'pulsar-pulse-functions)
      (dolist (fn '('outline-cycle-buffer
                    'toggle-input-method))
        (add-to-list 'pulsar-pulse-functions fn)))
    (defadvice! zy--pulse-a (fn)
      "Pulse the line with FN based on a set of rules."
      :around 'pulsar-pulse-line
      ;; Pulse the cursor in different colors based on the current IM.
      (dlet ((pulsar-face (if current-input-method
                              'pulsar-yellow
                            'pulsar-cyan)))
        (funcall fn)))))

;;;;; Emoji support

;; Displaying and inserting emojis.

(use-package emojify
  :straight t
  :hook (zy-first-buffer . global-emojify-mode)
  :general
  ;; Replace the default emoji shortcut.
  ("C-x 8 e" 'emojify-insert-emoji)
  :config
  (setq!
   ;; Display only Unicode emojis.
   emojify-emoji-styles '(unicode)
   ;; Display emojis with Unicode fonts.
   emojify-display-style 'unicode))

;;;;; Icon support

(unless *windows*
  ;; Provide icon support with Nerd Fonts.
  (use-package nerd-icons
    :straight t
    :config
    (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

  (use-package nerd-icons-dired
    :straight (nerd-icons-dired :type git :host github
                                :repo "rainstormstudio/nerd-icons-dired")
    :hook
    (dired-mode . nerd-icons-dired-mode))

  ;; Corfu support (together with Kind-icon).
  (use-package kind-icon
    :straight t
    :unless *windows*
    :after corfu nerd-icons
    :config
    (setq!
     ;; To compute blended backgrounds correctly.
     kind-icon-default-face 'corfu-default
     ;; Use Kind-icon as margin formatter.
     corfu-margin-formatters #'kind-icon-margin-formatter
     ;; Do not use SVG icons.
     kind-icon-use-icons nil
     ;; Use Nerd-icons instead.
     kind-icon-mapping
     `(
       (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
       (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
       (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
       (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
       (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
       (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
       (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
       (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
       (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
       (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
       (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
       (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
       (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
       (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
       (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
       (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
       (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
       (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
       (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
       (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
       (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
       (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
       (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
       (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
       (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
       (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
       (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
       (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
       (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
       (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
       (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
       (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
       (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
       (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
       (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
       (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))))

;;;;; No ringing the bell

;; Everytime I type C-g to stop something, it rings the bell.  This is normally
;; not a big deal, but I really hate the bell sound on Windows!  So instead of
;; ringing the bell, I make Emacs flash the mode line instead.

(defun zy-flash-mode-line ()
  "Flash the mode line."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq ring-bell-function #'zy-flash-mode-line)

;;;;; Darkroom (distraction-free mode)

(use-package darkroom
  :straight t
  :general
  (:keymaps 'zy-toggle-map
            "d" 'darkroom-tentative-mode)
  :config
  ;; Toggle `display-line-numbers-mode' and `hl-line-mode' on text mode buffers,
  ;; when switching darkroom mode.
  (defadvice! zy--darkroom-enter-a (&rest _)
    "Turn off UI elements when entering Darkroom in a text mode buffer."
    :after 'darkroom--enter
    (when (derived-mode-p 'text-mode)
      (hl-line-mode -1)
      (display-line-numbers-mode -1)))
  (defadvice! zy--darkroom-leave-a (&rest _)
    "Turn on UI elements when leaving Darkroom in a text mode buffer."
    :after 'darkroom--leave
    (when (derived-mode-p 'text-mode)
      (hl-line-mode 1)
      (display-line-numbers-mode 1))))

;;;;; Configure font-lock (jit-lock)

;; Configure Emacs's just-in-time font-locking (jit-lock).
(use-package jit-lock
  :defer t
  :init
   ;; Enable stealthy font locking.
  (setq
   ;; Calculate fonts when idle for 1.5 seconds.
   jit-lock-stealth-time 1.5
   ;; Pause for 0.2 seconds between calculations afterwards.
   jit-lock-stealth-nice 0.2))

;;;; Features

;; This section is for settings that provide additional features for Emacs.

;;;;; Lingual

;;;;;; Smart input source

(use-package sis
  :straight t
  :init
  (cond
   ;; Emacs-native IM on WSL.
   ((or *wsl* *windows*) (sis-ism-lazyman-config nil "rime" 'native))
   ;; Fcitx5 on Linux.
   (*linux* (sis-ism-lazyman-config "1" "2" 'fcitx5)))
  (setq!
   sis-other-cursor-color "orange")
  (sis-global-respect-mode 1)
  (sis-global-cursor-color-mode 1))

;;;;;; Rime input method

;; Rime is my favorite input method for every platforms.  Fortunately it is
;; integrated into Emacs as well.

(use-package rime
  :straight t
  :defer t
  :general
  ;; On Linux, I map Left Shift to F16 when pressed alone in Emacs via Xremap,
  ;; so that I can toggle input method conveniently inside Emacs.  However,
  ;; Emacs interprets F16 to <Launch7> (on Pure GTK) or <XF86Launch7> (on X), so
  ;; I have to map that.
  ;;
  ;; Why don't I map it to C-\ directly?  Because Xremap doesn't support that.
  ;; Besides, mapping it to a rarely-used key causes less side effects.
  ("<Launch7>" 'toggle-input-method
   "<XF86Launch7>" 'toggle-input-method)
  :init
  (setq!
   ;; Use Rime as the default input method.
   default-input-method "rime"
   ;; I have my scheme data in my emacs directory.
   rime-user-data-dir (expand-file-name "etc/rime" user-emacs-directory)
   ;; Show candidates in the minibuffer.  For Chinese users: 作爲倉頡輸入法用戶，
   ;; 我輸入漢字一般是逐字輸入，所以我沒有沒有「同步詞庫」的需求；而倉頡中每個漢
   ;; 字的編碼幾乎是唯一的，我幾乎可以不看候選盲打，所以在 minibuffer 中展示候選
   ;; 也不會影響我的打字。
   rime-show-candidate 'minibuffer)

  ;; On Windows, the share directory has to be manually set.
  (when (eq system-type 'windows-nt)
    (setq! rime-share-data-dir "c:/msys64/mingw64/share/rime-data")))

;;;;;; OpenCC (simplified/traditional chinese converter)

;; OpenCC is a handy tool for me.  It is not easy to install it on Windows, but
;; lucily Librime provides an executable of it.  However, the configuration
;; files have to be set manually on Windows.

(use-package opencc
  :straight t
  :commands (opencc-replace-at-point
             opencc-print-buffer
             opencc-insert-mode
             opencc-isearch-mode)
  :config
  ;; Set absolute path for configuration files on Windows.  For me, OpenCC is
  ;; always provided by Librime, which is installed by Scoop, so the path is
  ;; consistent.  But it should not work for you.
  (when (eq system-type 'windows-nt)
    (let ((opencc-path-prefix
           "C:\\Users\\zyxir\\scoop\\apps\\librime\\current\\share\\opencc\\"))
      (setq! opencc-configuration-files
             (mapcar (lambda (file)
                       (expand-file-name file opencc-path-prefix))
                     opencc-configuration-files)))))

;;;;; Syntax checker (Flymake and Flycheck)

(use-package flycheck
  :straight t
  ;; Enable Flycheck only when there is no Eglot support.
  :hook (emacs-lisp-mode sh-mode TeX-mode verilog-mode)
  :commands flycheck-mode
  :general
  (:keymaps 'flycheck-mode-map
            "M-p" 'flycheck-previous-error
            "M-n" 'flycheck-next-error)
  :config
  ;; Show a shorter mode lighter.
  (setq! flycheck-mode-line-prefix "Fc"))

(use-package flymake
  ;; Enable Flymake only for Eglot.
  :hook eglot-managed-mode-hook
  :general
  (:keymaps 'flymake-mode-map
            "M-p" 'flymake-goto-prev-error
            "M-n" 'flymake-goto-next-error)
  :config
  ;; Show a shorter mode lighter.
  (setq! flymake-mode-line-lighter "Fm"))

;;;;; Language server protocol support via Eglot

(use-package eglot
  :unless zy~use-lsp-bridge
  :straight '(eglot :type built-in)
  :general
  (:keymaps 'zy-coding-map
            "a" 'eglot-code-actions
            "f" 'eglot-format
            "r" 'eglot-rename
            "R" 'eglot-reconnect)
  :init
  ;; Always ensure Eglot after everything else.
  (add-hook!
      (c-mode-common python-base-mode scala-mode)
    :depth 100
    'eglot-ensure)

  (setq!
   ;; Do not need extra confirm on code actions.
   eglot-confirm-server-initiated-edits nil
   ;; Server-specific configuraration.
   eglot-workspace-configuration
   '(;; Python.
     :pylsp
     (:plugins
      (;; Type checking.
       :mypy (:enabled t)
       ;; Code formatting.
       :black (:enabled t)
       ;; Linting.
       :ruff (:enabled t)
       ;; Imports sorting.
       :pyls_isort (:enabled t))))))

;;;;; Lsp-bridge (optional, another LSP client)

(use-package lsp-bridge
  :when zy~use-lsp-bridge
  :straight '(lsp-bridge :repo "manateelazycat/lsp-bridge"
                         :files (:defaults "*.py" "acm" "core"
                                           "langserver" "multiserver"))
  'posframe 'markdown-mode 'yasnippet
  :hook (zy-first-file . global-lsp-bridge-mode)
  :general
  (:keymaps 'lsp-bridge-mode-map
            ;; Definition finding, overwriting Xref keys.
            "M-." 'lsp-bridge-find-def
            "M-," 'lsp-bridge-find-def-return
            "M-?" 'lsp-bridge-find-references
            "C-x 4 ." 'lsp-bridge-find-def-other-window
            ;; Documentation, overwriting Eldoc keys.
            "C-h ." 'lsp-bridge-popup-documentation
            ;; Diagnostics, overwirting Flymake or Flycheck keys.
            "M-n" 'lsp-bridge-diagnostic-jump-next
            "M-p" 'lsp-bridge-diagnostic-jump-prev
            )
  (:keymaps 'zy-coding-map
            "a" 'lsp-bridge-code-action
            "f" 'lsp-bridge-code-format
            "r" 'lsp-bridge-rename
            "R" 'lsp-bridge-restart-process
            "dl" 'lsp-bridge-diagnostic-list
            "dc" 'lsp-bridge-diagnostic-copy
            "di" 'lsp-bridge-diagnostic-ignore)
  :config
  (setq!
   ;; Do not use TabNine.
   acm-enable-tabnine nil
   ;; Do not enable yasnippet completion.  I prefer `consult-yasnippet' or manual trigger.
   acm-enable-yas nil
   ;; Enable quick accessing candidates with M-<number>.
   acm-enable-quick-access t
   ;; Use Pylsp for Python.
   lsp-bridge-python-lsp-server "pyright"
   lsp-bridge-python-multi-lsp-server "pyright_ruff"
   ;; Read user configuration.
   lsp-bridge-user-langserver-dir (expand-file-name "etc/langserver"
                                                    user-emacs-directory)))

;;;;; Shell and terminal

;;;;;; Comint and Shell

(use-package comint
  :defer t
  :config
  (setq!
   ;; Do not delete the prompt.
   comint-prompt-read-only t))

;;;;;; Eshell

;; Eshell is a consistent shell environment across platforms.
(use-package eshell
  :commands eshell
  :config
  (setq!
   ;; Scroll to the cursor on input.
   eshell-scroll-to-bottom-on-input 'this
   ;; Error if a glob pattern does not match, like Zsh.
   eshell-error-if-no-glob t
   ;; No duplicate history items.
   eshell-hist-ignoredups t
   ;; Save shell history.
   eshell-save-history-on-exit t
   ;; Prefer Lisp functions to external ones.
   eshell-prefer-lisp-functions nil
   ;; No need to keep the buffer after exit.
   eshell-destroy-buffer-when-process-dies t)

  ;; Aliases.
  (defalias 'eshell/e 'find-file)
  (defalias 'eshell/edit 'find-file)
  (defalias 'eshell/emacs 'find-file)
  (defalias 'eshell/ff 'find-file)
  (defalias 'eshell/ffow 'find-file-other-window)
  (defalias 'eshell/ffof 'find-file-other-frame)

  ;; New command: clear.
  (declare-function eshell-send-input "eshell")
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input))))

;;;;;; Vterm

;; Vterm is the best terminal emulator inside Emacs.  Currently Vterm doesn't work on
;; Windows.

(use-package vterm
  :straight t
  :commands vterm
  :general (:keymaps 'vterm-mode-map
                     "C-q" 'vterm-send-next-key))

;;;;;; Colorful Shell Output

(use-package xterm-color
  :straight t
  :after eshell
  :config
  (require 'eshell)
  (require 'esh-mode)
  ;; Configure Eshell.
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))

;;;;; Calendar

;; I use the Calendar mainly for Org-journal, so there is almost no
;; configuration here.

(use-package calendar
  :general
  ("C-c l" 'calendar))

;;;;; Emacs configuration management

;; Restart Emacs from withing Emacs.
(use-package restart-emacs
  :straight t
  :commands (restart-emacs restart-emacs-start-new-emacs))

(declare-function restart-emacs 'restart-emacs)
(declare-function restart-emacs-start-new-emacs 'restart-emacs)

(use-package zy-conf-man
  :defer t
  :init
  ;; Open another Emacs to test the current config.
  (defun zy/test-config (&rest _)
    "Start a new Emacs with \"--debug-init\" to test the config."
    (interactive)
    (restart-emacs-start-new-emacs '("--debug-init")))

  ;; Open another Emacs with "-Q".
  (defun zy/test-no-site-file (&rest _)
    "Start a new Emacs with \"-Q\"."
    (interactive)
    (restart-emacs-start-new-emacs '("-Q")))

  ;; Quickly open the config file.
  (defun zy/open-config (&rest _)
    "Open Emacs configuration in another window."
    (interactive)
    (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
      (find-file init-file)))

  (defun zy--rebuild-config (&rest _)
    "Thaw all packages and rebuild them."
    (when (fboundp 'straight-thaw-versions)
      (straight-thaw-versions))
    (when (fboundp 'straight-rebuild-all)
      (straight-rebuild-all)))

  (defun zy/rebuild-config (&rest _)
    "Start a new Emacs and rebuild all packages."
    (interactive)
    (restart-emacs-start-new-emacs '("--debug-init"
                                     "--eval"
                                     "(zy--rebuild-config)")))

  (general-def
    "C-c e e" 'zy/open-config
    "C-c e b" 'zy/rebuild-config
    "C-c e t" 'zy/test-config
    "C-c e q" 'zy/test-no-site-file
    "C-c e R" 'restart-emacs))

;;;;; Bibliography management

;; Manage ".bib" databases with Emacs.  These tools are in use: Bibtex
;; (built-in), Biblio, Citar, Citar-embark, Ebib.

(defvar zy-bib-files nil
  "All of my BibLaTeX databases.
Automatically set when `zy~zybox-dir' is customized.")

;; Inserting and managing citations with Citar.  Related directories are set
;; when `zy~zybox-dir' is customized.

(use-package citar
  :straight t
  :general
  ("C-c i" 'citar-insert-citation)
  :config
  (setq!
   ;; Open files externally.
   citar-file-open-functions '((t . zy/open-externally))))

;; Load Citar-embark when Embark is loaded, so that BibTeX keys can be
;; recognized.

(use-package citar-embark
  :straight t
  :after embark
  :no-require
  :config
  (citar-embark-mode))

;; Manage bibliography database in Emacs with Ebib.  Related directories are set
;; when `zy~zybox-dir' is customized.

(use-package ebib
  :straight t
  :general
  ("C-c b" 'ebib)
  (:keymaps 'ebib-index-mode-map
            "+" 'ebib-import-file)
  :init
  (defvar zy-ebib-bib-file nil
    "BibLaTeX database used by Ebib.
Automatically set when `zy~zybox-dir' is customized.")
  :config
  (setq!
   ;; Generate BibTeX keys automatically.
   ebib-auotgenerate-keys t
   ;; Open entry files externally.
   ebib-file-associations
   `(("caj" . zy/open-externally)
     ("pdf" . zy/open-externally)
     ("ps" . zy/open-externally))))

;; Import entry with DOI via Biblio.

(use-package biblio
  :straight t
  :general
  (:keymaps 'ebib-index-mode-map
            "B" 'ebib-biblio-import-doi)
  (:keymaps 'biblio-selection-mode-map
            "e" 'ebib-biblio-selection-import)
  :config
  (setq!
   ;; Generate keys automatically.
   biblio-bibtex-use-autokey t))

;; Automatic key generation with bibtex.

(use-package bibtex
  :defer t
  :config
  (setq!
   ;; Use biblatex dialect.
   bibtex-dialect 'biblatex
   ;; See `bibtex-generate-autokey'.
   bibtex-autokey-year-length 4
   bibtex-autokey-titlewords 2
   bibtex-autokey-name-year-separator "_"
   bibtex-autokey-year-title-separator "_"))

;;;;; Convert decorated text to HTML

;; Currently this package is only used by Org-reveal to fontify source blocks.

(use-package htmlize
  :straight t
  :commands (htmlize-file
             htmlize-buffer
             htmlize-region
             htmlize-many-files
             htmlize-many-files-dired
             htmlize-region-save-screenshot))

;;;;; OS-specific configuration

;;;;;; WSL (Windows subsystem for Linux)

(use-package zy-wsl
  :when *wsl*
  :demand t
  :no-require
  :config
  (defun zy/browse-url-explorer (url &rest _)
    "Open URL with the \"explorer.exe\" command.
Explorer.exe can call the default browser on the Windows system
that hosts WSL."
    (interactive)
    (call-process "explorer.exe" nil 0 nil url))
  (setq browse-url-browser-function 'zy/browse-url-explorer)
  (defun zy/open-file-with-explorer (file &rest _)
    "Open FILE with the \"explorer.exe\" command.
Path is converted with the \"wslpath\" command."
    (interactive)
    (let* ((file-abs (expand-file-name file))
           (wslpath-output (with-output-to-string
                             (with-current-buffer standard-output
                               (call-process "wslpath" nil standard-output
                                             nil "-w" file-abs))))
           (len (length wslpath-output))
           (converted-path (cond
                            ((and (> len 0) (eq (aref wslpath-output (- len 1)) ?\n))
                             (substring wslpath-output 0 (- len 1)))
                            (t wslpath-output))))
      (message converted-path)
      (call-process "explorer.exe" nil 0 nil converted-path))))

;;;; File type specific settings

;; This section enhances Emacs on specific file types, mostly programming
;; languages.

;;;;; Emacs Lisp

(use-package elisp-mode
  :general
  (:keymaps 'emacs-lisp-mode-map
            ;; A handy key to expand macros.
            "C-c C-x" 'emacs-lisp-macroexpand)
  :config
  (add-hook! emacs-lisp-mode
    'outline-minor-mode)
  (setq-hook! emacs-lisp-mode
    ;; Emacs Lisp code tends to be wide, so use a bigger fill-column.
    fill-column 80)

  (setq!
   ;; Let Flycheck inherit Emacs load path.
   flycheck-emacs-lisp-load-path 'inherit)

  ;; Use four semicolons as level 1.  Standard Emacs Lisp files always contain
  ;; some special comments starting with three semicolons, and I don't want to
  ;; treat them as level 1 outline.  So, I write my own outlines starting from
  ;; four semicolons.
  (defadvice! zy--lisp-outline-level ()
    "Customized approach to calculate Lisp outline level."
    (let ((len (- (match-end 0) (match-beginning 0))))
      (cond ((looking-at ";;;\\(;+\\) ")
             (- (match-end 1) (match-beginning 1)))
            ;; Above should match everything but just in case.
            (t len))))

  (setq-hook! 'emacs-lisp-mode-hook
    ;; Don't treat autoloads or sexp openers as outline headers.  Use
    ;; hideshow for that.
    outline-regexp "[ \t]*;;;;+ [^ \t\n]"
    outline-level 'zy--lisp-outline-level)

  ;; Flymake is good, but it crashes a lot on Windows.  These code are kept in
  ;; case someday I decide to switch back to it.
  (setq! elisp-flymake-byte-compile-load-path
         (delete-dups
          (append load-path
                  (default-toplevel-value
                   'elisp-flymake-byte-compile-load-path)))))

;;;;; JavaScript / JSON

(use-package js
  :defer t
  :config
  (add-hook! js-base-mode
    ;; Use 2 spaces to indent.
    (setq! js-indent-level 2)))

;;;;; Lua

(use-package lua-mode
  :straight t)

;;;;; Markdown

(use-package edit-indirect
  :straight t
  :commands (edit-indirect-region))

(use-package markdown-mode
  :straight t
  :magic ("\\.md\\|\\.markdown" . markdown-mode))

;;;;; Org

;; This part of the configuration is very extensive.

;;;;;; Org basic

;; Basic settings about Org itself, and some simpler extensions.

(use-package org
  :straight t
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-agenda
  org-capture
  :init
  (setq!
   ;; Indent sections by depth.
   org-startup-indented t)
  (add-hook! org-mode
    ;; Org is my main prose editor.  I prefer working with variable pitch fonts
    ;; when writing proses.
    'variable-pitch-mode
    ;; Turn off display-line-numbers-mode, as it breaks table display.
    (display-line-numbers-mode -1))

  ;; GTD files.
  (defvar zy-gtd-inbox-file nil
    "My inbox file of the GTD system.
Automatically set when `zy~zybox-dir' is customized.")
  (defvar zy-gtd-gtd-file nil
    "My GTD file of the GTD system.
Automatically set when `zy~zybox-dir' is customized.")
  (defvar zy-gtd-someday-file nil
    "My someday file of the GTD system.
Automatically set when `zy~zybox-dir' is customized.")

  ;; Other org files.
  (defvar zy-notes-file nil
    "My file of short notes.
Automatically set when `zy~zybox-dir' is customized.")

  :general
  ("C-c a" 'org-agenda
   "C-c c" 'org-capture)
  (:keymaps 'org-mode-map
            "M-g h" 'consult-org-heading)

  :config
  (setq!
   org-agenda-files (list zy-gtd-inbox-file zy-gtd-gtd-file)
   ;; My favorite attachment directory.
   org-attach-id-dir "_org-att"
   ;; Capture templates for the GTD system.
   org-capture-templates `(("i" "GTD inbox" entry
                            (file+headline ,zy-gtd-inbox-file "Inbox")
                            "* TODO %i%? %^G\nCREATED: %U"
                            :kill-buffer t)
                           ("n" "Write a note" entry
                            (file+headline ,zy-notes-file "Notes")
                            "* %i%? \nCREATED: %U"
                            :empty-lines 1
                            :prepend t
                            :kill-buffer t))
   ;; Hide emphasis markers.
   org-hide-emphasis-markers t
   ;; Less indentation for `org-indent-mode'.
   org-indent-indentation-per-level 1
   ;; Track the time of various actions.
   org-log-done 'time
   ;; Refile from inbox to other GTD files.
   org-refile-targets `((,zy-gtd-gtd-file :level . 2)
                        (,zy-gtd-someday-file :maxlevel . 3))
   ;; This set of keywords works for me.
   org-todo-keywords '((sequence "TODO(t)"
                                 "DOING(i)"
                                 "|"
                                 "DONE(d)"
                                 "CANCELED(c)"))
   org-todo-keyword-faces '(("TODO" . org-todo)
                            ("DOING" . (:foreground "#00bcff"))
                            ("DONE" . org-done)
                            ("CANCELED" . shadow)))

  ;; Setup faces.
  (defun zy--setup-org-faces (&rest _)
    "Setup faces for Org mode."
    ;; Use monospaced font for code and blocks.
    (set-face-attribute 'org-block nil :family "Sarasa Mono HC"))
  (zy--setup-org-faces)
  (add-hook 'zy-load-theme-hook 'zy--setup-org-faces))

;; Setup Org-babel with some additional languages.
(use-package ob
  :defer t
  :config
  (defvar org-babel-load-languages)
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '(;; For literate scripting.
             (shell . t)
             ;; Also for literate scripting.
             (python . t)))))

;; Use Org tempo for block expansion.
(use-package org-tempo
  :after org)

;; Show emphasis markers while inside it.
(use-package org-appear
  :straight t
  :hook org-mode)

;;;;;; Org export common settings

(use-package ox
  :defer t
  :config
  (setq!
   ;; Do not export TOC or tags, unless asked to.
   org-export-with-toc nil
   org-export-with-tags nil
   ;; Normally I don't use "_" or "^" in Org files, and exporting them as
   ;; sub/superscripts will mess up terms like "Pop!_OS".  So I disable this,
   ;; and it can always be toggled file-wise if really needed.
   org-export-with-sub-superscripts nil))

;;;;;; Org export to HTML

(use-package ox-html
  :defer t
  :config
  ;; MHTML exporter that embeds images.
  ;; See https://niklasfasching.de/posts/org-html-export-inline-images/

  (declare-function org-export-define-derived-backend "ox")
  (declare-function org-combine-plists "org-macs")
  (declare-function org-html-close-tag "ox-html")
  (declare-function org-html-export-to-html "ox-html")
  (declare-function org-html--make-attribute-string "ox-html")

  (defun org-html-export-to-mhtml (async subtree visible body)
    (cl-letf (((symbol-function 'org-html--format-image)
               'format-image-inline))
      (org-html-export-to-html async subtree visible body)))

  (defun format-image-inline (source attributes info)
    (let* ((ext (file-name-extension source))
           (prefix (if (string= "svg" ext)
                       "data:image/svg+xml;base64,"
                     "data:;base64,"))
           (data (with-temp-buffer (url-insert-file-contents source)
                                   (buffer-string)))
           (data-url (concat prefix (base64-encode-string data)))
           (attributes (org-combine-plists
                        `(:src ,data-url) attributes)))
      (org-html-close-tag
       "img"
       (org-html--make-attribute-string attributes)
       info)))

  (org-export-define-derived-backend 'html-inline-images 'html
                                     :menu-entry '(?h
                                                   "Export to HTML"
                                                   ((?m "As MHTML file" org-html-export-to-mhtml)))))

;;;;;; Org export to LaTeX

(use-package ox-latex
  :defer t
  :config
  ;; Get the zylatex.sty file.

  (defvar zy-zylatex-file
    (expand-file-name "etc/zylatex.sty" user-emacs-directory)
    "My personal LaTeX style file.")

  (unless (file-exists-p zy-zylatex-file)
    (condition-case-unless-debug e
        (when (fboundp 'zy/update-zylatex-file)
          (zy/update-zylatex-file))
      (error
       (message "Error fetching \"zylatex.sty\" because %s." e)
       (setq zy-zylatex-file nil))))

  ;; Configure Org to LaTeX export

  (setq! org-latex-compiler "xelatex"
         org-latex-default-class "article"
         ;; Delete ".tex" file as well.
         org-latex-logfiles-extensions
         '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx" "log"
           "nav" "out" "ptc" "run.xml" "snm" "tex" "toc" "vrb" "xdv"))

  (when zy-zylatex-file
    (setq! org-latex-classes
           `((;; 自用導出配置，用於個人日誌、散文等。
              "article"
              ,(format "\
\\documentclass[12pt]{article}
\\usepackage[]{%s}
[PACKAGES]
[EXTRA]" (file-name-sans-extension zy-zylatex-file))
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             (;; 適合手機屏幕閱讀的配置。
              "article-phone"
              ,(format "\
\\documentclass[12pt]{article}
\\usepackage[layout=phone]{%s}
[PACKAGES]
[EXTRA]" (file-name-sans-extension zy-zylatex-file))
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             (;; 適用於簡體中文的配置。
              "article-sc"
              ,(format "\
\\documentclass[12pt]{article}
\\usepackage[style=tc, fontset=ctex]{%s}
[PACKAGES]
[EXTRA]" (file-name-sans-extension zy-zylatex-file))
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

  ;; Export smartphone-friendly PDF

  (declare-function org-export-define-derived-backend "ox")
  (declare-function org-latex-export-to-pdf "ox-latex")
  (defun zy/org-export-to-pdf-phone
      (&optional async subtreep visible-only body-only ext-plist)
    "Export current buffer to smartphone-friendly PDF.

The function works like `org-latex-export-to-pdf', except that
`org-latex-default-class' is set to \"article-phone\"."
    (dlet ((org-latex-default-class "article-phone"))
      (org-latex-export-to-pdf async subtreep visible-only
                               body-only ext-plist)))

  (org-export-define-derived-backend
   'latex-pdf-phone 'latex
   :menu-entry '(?l
                 "Export to LaTeX"
                 ((?j "As PDF file (phone-friendly)"
                      zy/org-export-to-pdf-phone)))))

;;;;;; Org reveal

;; Org-reveal integrate Reveal.js into Org.

(use-package org-reveal
  :straight t
  :after ox
  :no-require t
  :config
  (require 'ox-reveal)
  (setq!
   ;; Reveal.js should be installed here.
   org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

;;;;;; Org journal

(use-package org-journal
  :straight t
  :after calendar
  :preface
  ;; Use `general-def' and :preface keyword here instead of the :general
  ;; keyword, so that Org-journal can be loaded after calendar is loaded.
  ;; Otherwise, Org-journal will only load after both `calendar' is loaded and
  ;; `org-journal-new-entry' is called.
  (general-def "C-c j" 'org-journal-new-entry)
  :init
  (setq!
   ;; In `org-journal-mode', use the original "C-c j" key as a prefix.
   org-journal-prefix-key "C-c j")
  :config
  (setq!
   ;; The following settings are applied for all my old journals, and work well
   ;; for me.  I tend not to change them, even if they are not the best.
   org-journal-extend-today-until 3
   org-journal-file-format "%F.org"
   org-journal-date-format "%F %a W%V\n"
   org-journal-date-prefix "#+title: "
   org-journal-time-format "%R "
   org-journal-time-format-post-midnight "%R (midnight) "
   org-journal-time-prefix "\n* "
   org-journal-file-header ""))

;;;;;; Org Roam

(use-package org-roam
  :straight t
  :general
  ("C-c r f" 'org-roam-node-find)
  :config
  (general-def
    :prefix "C-c r"
    "i" 'org-roam-node-insert
    "c" 'org-roam-capture
    "a" 'org-roam-alias-add
    "l" 'org-roam-buffer-toggle
    "s" 'org-roam-db-sync)
  (org-roam-db-autosync-mode 1))

(use-package org-roam-ui
  :straight t
  :after org-roam
  :general
  ("C-c r u" 'org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;;;;; PDF

(use-package pdf-tools
  :straight t
  :defer t
  :init
  (pdf-loader-install))

;;;;; PowerShell

;; This mode is useful on Windows, especially for its `powershell' command.

(use-package powershell
  :straight t
  :commands (powershell))

;;;;; Python

(use-package python
  :defer t
  :general
  (:keymaps 'python-base-mode-map
            "C-c C-a" 'zy/run-python-script)
  :config
  (setq!
   ;; See the documentation.
   python-fill-docstring-style 'pep-257-nn
   ;; No additional indent for def blocks, in accordence with the black convention.
   python-indent-def-block-scale 1
   ;; Use the default executable for the shell.
   python-shell-interpreter "python")

  ;; Fix the <backspace> key in Python.  I want <backspace> always perform `delete-region'
  ;; when there is an active region, but `python-indent-dedent-line-backspace' doesn't.
  (declare-function python-indent-dedent-line "python")
  (defadvice! zy--python-indent-dedent-line-backspace-a (arg)
    "Do backspace, or de-indent current line.
This is like `python-indent-dedent-line-backspace', but always
perform `backward-delete-char-untabify' when there is an active
buffer.

This overrides `python-indent-dedent-line-backspace'."
    :override 'python-indent-dedent-line-backspace
    (interactive "*p")
    (when (or (region-active-p) (not (python-indent-dedent-line)))
      (backward-delete-char-untabify arg))))

;; Enable running Pytest with the `python-pytest' command.
(use-package python-pytest
  :straight '(python-pytest :host github :repo "wbolster/emacs-python-pytest"
                            :fork (:repo "zyxir/emacs-python-pytest"
                                         :branch "dev" :protocol ssh))
  :general (:keymaps 'python-base-mode-map
                     [remap zy/test] 'python-pytest-dispatch))

;; Pet, the Python executable tracker, which automatically detects a Python
;; virtual environment and apply it to various Python packages like
;; python-pytest, python-isort and built-in project.el.  Pity it is not in
;; MELPA.
(use-package pet
  :straight '(pet :host github :repo "wyuenho/emacs-pet")
  :hook (python-base-mode))

;; Syntax highlight for requirements.txt.
(use-package pip-requirements
  :straight t
  :defer t
  :config
  (add-hook 'pip-requirements-mode-hook
            #'pip-requirements-auto-complete-setup))

;; Python-docstring provides syntax highlighting and filling command for reStructuredText
;; and Epydoc docstrings.
(use-package python-docstring
  :straight '(python-docstring :host github :repo "glyph/python-docstring-mode"
                               :fork (:repo "zyxir/python-docstring-mode"
                                            :branch "dev" :protocol ssh))
  :hook python-base-mode)

;;;;; Scala

(use-package scala-mode
  :straight t
  :interpreter ("scala" . scala-mode)
  :config
  (setq-hook! scala-mode
    fill-column 100))

(use-package sbt-mode
  :straight t
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;;;;; Shell scripts

(use-package sh-script
  :config
  (add-hook! sh-mode
    'outline-minor-mode))

;;;;; TeX / LaTeX

(use-package auctex
  :straight t
  :defer t)

(use-package tex
  :defer t
  :config
  (setq!
   ;; Do not fontify superscripts and subscripts.
   font-latex-fontify-script nil
   ;; Automatically save style information when saving the buffer.
   TeX-auto-save t
   ;; Parse file after loading it if no style hook is found for it.
   TeX-parse-self t
   ;; Do not ask the user to save the file.  Do it automatically.
   TeX-save-query nil
   ;; TeX-engine decides what command "LaTeX" will run.  At most time I prefer the XeTeX
   ;; engine as it handles CJK characters well.  However sometimes other engines should be
   ;; used (for example, the IEEEtran document class should be compiled with the default
   ;; engine), so this should be set at a per-file basis.
   TeX-engine 'xetex)

  (declare-function TeX-source-correlate-mode 'tex)
  (add-hook! TeX-mode
    ;; Enable inverse search.
    'TeX-source-correlate-mode)

  ;; Revert document buffer after compilation.
  (declare-function TeX-revert-document-buffer "tex")
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

(use-package reftex
  :after tex
  :demand t
  :config
  ;; Always turn on RefTeX while writting LaTeX.
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (with-eval-after-load 'reftex
    (setq!
     ;; Integrate with AUCTeX.
     reftex-plug-into-AUCTeX t
     ;; Reparse only 1 file when asked to.
     reftex-enable-partial-scans t
     ;; Save parsed information.
     reftex-save-parse-info t
     ;; Use a separate selection buffer for each label type.
     reftex-use-multiple-selection-buffers t)))

;;;;; Verilog

(use-package verilog-mode
  ;; Verilog mode is built-in, but I want to install the latest version, as
  ;; suggested by the official repository.
  :straight t
  :magic ("\\.v" . verilog-mode)
  :config
  (setq! verilog-auto-delete-trailing-whitespace t
         verilog-auto-newline nil
         verilog-case-level 2
         verilog-indent-begin-after-if nil
         verilog-indent-level 2
         verilog-indent-level-behavioral 0
         verilog-indent-level-declaration 0
         verilog-indent-level-module 0
         ;; Lint with Icarus Verilog.
         verilog-linter "iverilog"))

;;;;; Others

(use-package yaml-mode
  :straight t
  :commands yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode)))

(use-package nginx-mode
  :straight t
  :commands nginx-mode
  :init
  (add-to-list 'auto-mode-alist '("/nginx/.+\\.conf\\'" . nginx-mode)))

;;;; The end

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; fill-column: 90
;; End:
