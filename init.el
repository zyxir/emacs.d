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

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'cl-lib)

;;; Preparations

;;;; Check minimum version

;; Check version at both compile and runtime.
(eval-and-compile
  ;; The minimum version to run this configuration is 28.1.
  (when (< emacs-major-version 28)
    (user-error
     "Emacs version is %s, but this config requires 28.1 or newer"
     emacs-version)))

;;;; Startup hacks

;; Some hacks that make startup faster.  Most of them are learnt from Doom
;; Emacs, so thank you Henrik Lissner!

;;;;; Adjust garbage collection

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

;;;;; Unset file name handlers

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

;;;;; Reduce GUI noises

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

;;; Emacs Lisp enhancements

;; These functions and macros make writting this configuration much easier.
;; Many of them are copied or adapted from Doom Emacs.

;;;; Logging

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

;;;; Symbol manipulation

;; This is copied from Doom Emacs.
(defun zy-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

;; This is copied from Doom Emacs.
(defun zy-keyword-intern (str)
  "Convert STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

;;;; Enhance the hook system

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

;; This is copied from Doom Emacs.
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

(advice-add #'run-hooks :override #'zy-run-hooks)

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

;;;; Macros as syntactic sugars

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
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function
			  ,(if append-p :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append-p))))))

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

;; This is based on `add-hook!' from Doom Emacs.
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
in a lambda)."
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hooks (if (and (eq (car-safe hooks) 'quote)
			 (atom (cadr hooks)))
		    (list (cadr hooks))
		  (cadr hooks)))
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
		     (list 'function (cadr next)))
		    (t (prog1 `(lambda (&rest _) ,@(cons next rest))
			 (setq rest nil))))
	      func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook ',hooks)
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
  `(add-hook! ,hooks (setq-local ,@rest)))

;;; Top level utilities

;; Top levels stuffs that should be loaded before anything else.

;;;; Global variables, customizables and customization groups

(defgroup zyxir nil
  "Zyxir's customization layer over Emacs."
  :group 'emacs)

;;;; Custom hooks

;;;;; Switch buffer/window/frame hook

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

;;;;; First buffer hook

(defcustom zy-first-buffer-hook nil
  "Hooks run before the first interactively opened buffer."
  :type 'hook
  :local 'permenant-local
  :group 'zyxir)

(zy-run-hook-on 'zy-first-buffer-hook
		'(find-file-hook zy-switch-buffer-hook))

;;;;; First file hook

(defcustom zy-first-file-hook nil
  "Hooks run before the first interactively opened file."
  :type 'hook
  :local 'permenant-local
  :group 'zyxir)

(zy-run-hook-on 'zy-first-file-hook
		'(find-file-hook dired-initial-position-hook))

;;;; Incremental loading

;; I tried to design my own incremental loader at version 4.0, but it turned out
;; to be too complicated.  Finally I decided to adopt Doom Emacs's code.

;; This is copied from Doom Emacs.
(defvar zy-incremental-packages '(t)
  "A list of packages to load incrementally after startup.

Any large packages here may cause noticeable pauses, so it's
recommended you break them up into sub-packages.  For example,
`org' is comprised of many packages, and can be broken up into:

  (zy-load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))

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

;;;; Straight as the package manager

(setq-default
 ;; Cache autoloads into a single file to speed up startup.
 straight-cache-autoloads t
 ;; Use different build directories for different versions of Emacs to cope with
 ;; byte code incompatibility.
 straight-build-dir (format "build-%s" emacs-version)
 ;; Do not check for modifications, until explicitly asked to.
 straight-check-for-modifications '(find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(require 'straight)

;;;; Use-package (isolate package configurations)

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

;;;;; The :defer-incrementally keyword

;; This is adapted from Doom Emacs.

(with-eval-after-load 'use-package-core
  ;; Functions provided by `use-package-core'.
  (declare-function use-package-list-insert "use-package")
  (declare-function use-package-normalize-symlist "use-package")
  (declare-function use-package-process-keywords "use-package")

  ;; Add the keyword to the list.
  (push ':defer-incrementally use-package-deferring-keywords)
  (setq use-package-keywords
        (use-package-list-insert :defer-incrementally
				 use-package-keywords :after))

  ;; The normalizer and the handler.
  (defalias 'use-package-normalize/:defer-incrementally
    #'use-package-normalize-symlist)
  (defun use-package-handler/:defer-incrementally
      (name _keyword targets rest state)
    (use-package-concat
     `((zy-load-packages-incrementally
	',(if (equal targets '(t))
	      (list name)
	    (append targets (list name)))))
     (use-package-process-keywords name rest state))))

;;;; General as the keybinding manager

(use-package general
  :straight t
  :demand t)

;;;; Load Path

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Overall enhancements

;; Enhance Emacs in various ways.  Many of these settings are hard to
;; categorize, so I just put them here.

;;;; A bunch of setqs

(setq!
 ;; There should be no disabled commands.
 disabled-command-function nil
 ;; 80 is a sane default.  Recommended by Google.
 fill-column 80
 ;; My AutoHotkey scripts recognize my Emacs window by this title.
 frame-title-format '("" "ZyEmacs" " [%b]")
 ;; See the documentation.
 inhibit-compacting-font-caches t
 ;; This makes inter-process communication faster
 read-process-output-max (* 1024 1024)
 ;; Uniquify buffer names in a sane way.
 uniquify-buffer-name-style 'forward
 ;; Never use dialog boxes.
 use-dialog-box nil
 ;; Allow breaking after CJK characters.
 word-wrap-by-category t
 ;; Do not report native compilation warnings and errors.
 native-comp-async-report-warnings-errors nil)

;;;; No auto save or backup files

(use-package files
  :init
  ;; Make no auto save or backup files.
  (setq! auto-save-default nil
	 make-backup-files nil))

;;;; Automatically reverting file-visiting buffers

(use-package autorevert
  :hook (zy-first-file . global-auto-revert-mode)
  :config
  ;; Do not auto revert some modes.
  (setq! global-auto-revert-ignore-modes '(pdf-view-mode)))

;;;; Record recently opened files

(use-package recentf
  :defer-incrementally easymenu tree-widget timer
  :hook (zy-first-file . recentf-mode)
  :commands recentf-open-files
  :config
  (setq!
   ;; Do not do auto cleanups except its a daemon session.
   recentf-auto-cleanup (if (daemonp) 300)
   ;; Default is 20, which is far from enough.
   recentf-max-saved-items 200)

  (add-hook! '(zy-switch-window-hook write-file-functions)
    (defun zy--recentf-touch-buffer-h ()
      "Bump file in recent file list when it is switched to or written to."
      (when buffer-file-name
	(recentf-add-file buffer-file-name))
      ;; Return nil for `write-file-functions'
      nil))

  ;; Clean up recent files when quitting Emacs.
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

;;;; Highlight the current line

(use-package hl-line
  :hook (zy-first-buffer . global-hl-line-mode)
  :config
  ;; Only highlight line in the current window.
  (setq! global-hl-line-sticky-flag nil))

;;;; Persist variables across sessions

;; This is adopted from Doom Emacs.
(use-package savehist
  ;; persist variables across sessions
  :defer-incrementally custom
  :init (savehist-mode 1)
  :config
  (setq! savehist-save-minibuffer-history t
         savehist-autosave-interval nil     ; save on kill only
         savehist-additional-variables
         '(kill-ring                        ; persist clipboard
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
    (defun zy-savehist-remove-unprintable-registers-h ()
      "Remove unwriteable registers (e.g. containing window configurations).

Otherwise, `savehist' would discard `register-alist' entirely if
we don't omit the unwritable tidbits."
      ;; Save new value in the temp buffer savehist is running
      ;; `savehist-save-hook' in. We don't want to actually remove the
      ;; unserializable registers in the current session!
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))))

;;;; Isearch (incremental searching)

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

;;;; Encoding

;; Set everything to UTF-8.

(set-language-environment "UTF-8")

;;;; Line numbers

;; Line numbers display.
(use-package display-line-numbers
  :init
  ;; Explicitly define a width to reduce the cost of on-the-fly computation.
  (setq-default display-line-numbers-width 3)

  ;; Show absolute line numbers for narrowed regions to make it easier to tell
  ;; the buffer is narrowed, and where you are, exactly.
  (setq-default display-line-numbers-widen t)

  ;; Enable line numbers for these modes only.
  (add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
    #'display-line-numbers-mode))

;;;; Other inbuilt modes

;; Show column number on the mode line.
(column-number-mode 1)

;; Delete the active region on insersion.
(delete-selection-mode 1)

;; Toggle subword movement and editing.
(global-subword-mode 1)

;; Proper positioning of line breaks.
(require 'kinsoku)

;;; User interface

;;;; Use Modus themes by Protesilaus Stavrou

;; Protesilaus Stavrou is a really cool bro.

(use-package modus-themes
  :straight t
  :demand t
  :config
  (setq!
   modus-themes-italic-constructs t
   modus-themes-bold-constructs t
   ;; Headings are not sized for Modus Themes shipped with Emacs 28
   ;; Maybe I should use the non-built-in version instead
   modus-themes-headings '((0 . (background 1.2))
			   (1 . (background overline 1.3))
			   (2 . (background overline 1.2))
			   (3 . (background overline 1.1))
			   (4 . (background 1.1))
			   (t . (background regular 1.0)))
   modus-themes-hl-line '(intense)
   modus-themes-markup '(background intense)
   modus-themes-mixed-fonts t
   modus-themes-region '(accented no-extend)
   modus-themes-org-blocks '(gray-background)
   modus-themes-prompts '(background))
  (load-theme 'modus-vivendi 'no-confirm))

;;;; Set font for various faces

;; Font setter for other character sets

(defvar zy--fontset-cnt 0
  "Number of fontsets generated by `zy-set-face-charset-font'.")

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
		  (set-fontset-font fontset c font frame))
		charset)
	(set-fontset-font fontset charset font frame))
      ;; Assign the fontset to the face if necessary
      (when unspecified-p
	(set-face-attribute face frame :fontset fontset)))))

(defconst zy-cjk-charsets '(han cjk-misc bopomofo kana hangul)
  "CJK character sets.")

;; Font faces setup

(defcustom zy-font-size 18
  "The pixel size of font in `default' face."
  :type 'integer
  :group 'zyemacs)

(defface zy-sans nil
  "Sans-serif font face."
  :group 'zyemacs)

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
  ;; Default face
  (set-face-attribute 'default nil
		      :font (font-spec :family "Sarasa Mono CL"
				       :size zy-font-size))
  (zy-set-face-charset-font 'default nil zy-cjk-charsets "Sarasa Mono CL")
  ;; Fixed-pitch face
  (set-face-attribute 'fixed-pitch nil :font "Sarasa Mono CL"
		      :height 'unspecified)
  (zy-set-face-charset-font 'fixed-pitch nil zy-cjk-charsets "Sarasa Mono CL")
  ;; ZyEmacs sans-serif face
  (set-face-attribute 'zy-sans nil :font "Roboto")
  (zy-set-face-charset-font 'zy-sans nil zy-cjk-charsets "Sarasa Mono CL"))

(defun zy-maybe-setup-font-faces (&rest _)
  "Try to setup font faces.

If GUI is not available currently, add itself to
`after-make-frame-functions', so that it can be run again the
next time a frame is created.

If GUI is available, setup font with `zy/setup-font-faces', and
remove itself from `after-make-frame-functions' if it is there."
  (if (display-graphic-p)
      (prog1
	  (zy/setup-font-faces)
	(remove-hook 'after-make-frame-functions #'zy-maybe-setup-font-faces))
    (add-hook 'after-make-frame-functions #'zy-maybe-setup-font-faces)))

(zy-maybe-setup-font-faces)

;;; Programming languages

;;;; Emacs Lisp

(autoload 'zy-lisp-indent-function "zyutils" nil nil 'function)

(use-package elisp-mode
  :config
  (setq-hook! 'emacs-lisp-mode-hook
    ;; Don't treat autoloads or sexp openers as outline headers.  Use
    ;; hideshow for that.
    outline-regexp "[ \t]*;;;;*[^ \t\n]")

  ;; Proper indent function.
  (advice-add #'lisp-indent-function :override 'zy-lisp-indent-function))

(provide 'init)
;;; init.el ends here
