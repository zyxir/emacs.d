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

;;;; Preparations

;; Preparations that must be evaluated before all other things.

;;;;; Check minimum version

;; Check version at both compile and runtime.
(eval-and-compile
  ;; The minimum version to run this configuration is 28.1.
  (when (< emacs-major-version 28)
    (user-error
     "Emacs version is %s, but this config requires 28.1 or newer"
     emacs-version)))

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

;;;; Additional functions & macros

;; These functions and macros make writting this configuration much easier.
;; Many of them are copied or adapted from Doom Emacs.

;;;;; Logging

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

;;;;; Symbol manipulation

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

;;;;; Enhanced hook system

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

;;;;; Macros as syntactic sugars

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

;; This is copied from Doom Emacs.
(defun zy--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

HOOKS is either an unquoted mode, an unquoted list of modes, a
quoted hook variable or a quoted list of hook variables."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (zy-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

;; This is adapted from Doom Emacs.
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
  (declare (indent defun) (debug t))
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
		     (list 'function (cadr next)))
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
           (advice-add target (car targets) #',symbol))))))

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

;;;; Top level utilities

;; Top levels utilities that should be loaded before other customizations.

;;;;; Global variables and customizations

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
`org-journal-directory', is decided by this function as well."
  ;; Set the value of `sym' to `path'.
  (set sym path)
  ;; Set other directories only when `path' is a valid directory.
  (when (file-directory-p path)
    ;; The Org directory.
    (setq-default org-directory (expand-file-name "org" path))
    ;; My GTD directory.
    (setq-default zy-gtd-dir (expand-file-name "org-gtd" org-directory))
    ;; My Org-journal directory.
    (setq-default org-journal-dir
		  (expand-file-name "org-journal" org-directory))
    ;; My Org-roam directory.
    (setq-default org-roam-directory
		  (expand-file-name "org-roam" org-directory))))

(defcustom zy~zybox-dir ""
  "The Zybox directory, my personal file center."
  :group 'zyxir-paths
  :type 'directory
  :set #'zy--set-zybox-path)

(defvar zy-gtd-dir nil
  "The GTD directory with my getting-things-done files.")

;;;;; Custom hooks

;;;;;; Switch buffer/window/frame hook

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

;;;;;; First buffer hook

(defcustom zy-first-buffer-hook nil
  "Hooks run before the first interactively opened buffer."
  :type 'hook
  :local 'permenant-local
  :group 'zyxir)

(zy-run-hook-on 'zy-first-buffer-hook
		'(find-file-hook zy-switch-buffer-hook))

;;;;;; First file hook

(defcustom zy-first-file-hook nil
  "Hooks run before the first interactively opened file."
  :type 'hook
  :local 'permenant-local
  :group 'zyxir)

(zy-run-hook-on 'zy-first-file-hook
		'(find-file-hook dired-initial-position-hook))

;;;;; Incremental loading

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

;;;;; Straight as the package manager

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

;;;;; Use-package (isolate package configurations)

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
                       (let ((default-directory doom-emacs-dir))
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

;;;;; Key binding

;;;;;; General as the key binding manager

(use-package general
  :straight t
  :demand t)

;;;;;; Hydra provides another key binding style

;; A Hydra is something works like "C-x C-=": after the first key combination,
;; you can only press single "+" or "-" to further adjusting the font size.

(use-package hydra
  :straight t
  :demand t)

;;;;;; Global keymaps

;; The toggle map.

(defvar zy-toggle-map (make-sparse-keymap)
  "Keymap for toggling various options.")
(fset 'zy-toggle-command zy-toggle-map)
(general-def "C-c t" 'zy-toggle-command)

;;;;; Load Path

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;; Base settings

;; This section contains bottom layer settings that control how Emacs works.

;;;;; Better defaults

(setq!
 ;; Save customizations outside the init file.
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 ;; As an experienced Emacs user, I don't want any command disabled.
 disabled-command-function nil
 ;; A informational frame title.  Besides, my AutoHotkey scripts recognize my
 ;; Emacs window by the \"ZyEmacs\" prefix.
 frame-title-format '("ZyEmacs@"
		      (:eval (or
			      (file-remote-p default-directory 'host)
			      system-name))
		      " [%b]")
 ;; I usually use a Hybrid font like Sarasa Gothic, which contains tremendous
 ;; amout of CJK glyphs.  Disable compacting of font makes redisplay faster.
 inhibit-compacting-font-caches t
 ;; Do not report native compilation warnings and errors.  Those do not matter
 ;; in most occasions.
 native-comp-async-report-warnings-errors nil
 ;; The default (4 kB) is too low considering some language server responses are
 ;; in 800 kB to 3 MB.  So it is set to 1 MB as suggested by Lsp-mode.
 read-process-output-max (* 1024 1024)
 ;; Uniquify buffer names in a saner way.
 uniquify-buffer-name-style 'forward
 ;; Never use dialog boxes.
 use-dialog-box nil)

;;;;; No auto save or backup files

(use-package files
  :init
  ;; Make no auto save or backup files.
  (setq! auto-save-default nil
	 make-backup-files nil))

;;;;; Encoding

;; Set everything to UTF-8.

(set-language-environment "UTF-8")

;; Encoding hacks on Microsoft Windows in Chinese locale.

(when (and
       ;; The operating system is Microsoft Windows.
       (eq system-type 'windows-nt)
       ;; Code page 936, the character encoding for simplified Chinese.
       (eq locale-coding-system 'cp936))
  ;; Use GBK for cmdproxy.exe and plink.exe.
  (set-default 'process-coding-system-alist
     '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
       ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos))))

;;;;; Emacs server

(use-package server
  :defer 1
  :config
  ;; Start the server if possible.
  (unless (and (fboundp 'server-running-p)
	       (server-running-p))
    (server-start)))

;;;;; Terminal settings

(add-hook! tty-setup
  ;; Let Emacs handle mouse events by default.  However, normal xterm mouse
  ;; functionality is still available by holding Shift key.
  (xterm-mouse-mode 1))

;;;; Text-editing

;; This section enhances the basic text-editing capability of Emacs.

;;;;; Isearch (incremental searching)

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
  :init
  (setq!
   ;; 80 is a sane default.  Recommended by Google.
   fill-column 80
   ;; These settings are adapted from Protesilaus Stavrou's configuration.
   sentence-end-double-space t
   sentence-end-without-period nil
   colon-double-space nil
   use-hard-newlines nil
   adaptive-fill-mode t))

;;;;; Outline minor mode for buffer structuring

;; Hook `outline-minor-mode' to specific language modes.

(use-package outline
  :commands (outline-minor-mode)
  :config
  (setq!
   ;; Cycle outline visibility with TAB.
   outline-minor-mode-cycle t
   ;; Highlight outline headings.
   outline-minor-mode-highlight 'append))

;;;;; Whitespaces

(use-package whitespace
  :defer t
  :init
  ;; Show trailing whitespace for all kinds of files.
  (add-hook! (prog-mode text-mode)
    (setq-local show-trailing-whitespace t)))

;;;;; Word wrapping and visual lines

;; Word wrapping is often enabled by `visual-line-mode', so we defer loading of
;; these configurations until it being run.

(add-transient-hook! `visual-line-mode
  (setq!
   ;; Allow line breaking after CJK characters.  Setting this with `setq!' will
   ;; load kinsoku.el automatically, which enhances line breaking.
   word-wrap-by-category t))

;;;;; Clipboard and kill ring

(use-package select
  :init
  ;; Save existing clipboard text into kill ring before replacing it.  This
  ;; saves me so many time!
  (setq save-interprogram-paste-before-kill t))

;; Clipetty provides clipboard access in Terminal.  It sends killed text to the
;; system clipboard.  However, copying from clipboard can only be done by GUI
;; commands (like "Ctrl+Shift+V" on most terminal emulators).
(unless (display-graphic-p)
  (use-package clipetty
    :straight t
    :hook tty-setup))

;;;;; Other inbuilt modes

;; Toggle subword movement and editing.
(global-subword-mode 1)

;;;; Workbench

;; This section contains settings about files, directories, buffers, windows,
;; frames, and other things about the "workbench".

;;;;; Autorevert (automatically refresh file-visiting buffers)

(use-package autorevert
  :hook (zy-first-file . global-auto-revert-mode)
  :config
  ;; Do not auto revert some modes.
  (setq! global-auto-revert-ignore-modes '(pdf-view-mode)))

;;;;; Savehist (persist variables across sessions)

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
    (defun zy-savehist-remove-unprintable-registers-h ()
      "Remove unwriteable registers (e.g. containing window configurations).

Otherwise, `savehist' would discard `register-alist' entirely if
we don't omit the unwritable tidbits."
      ;; Save new value in the temp buffer savehist is running
      ;; `savehist-save-hook' in. We don't want to actually remove the
      ;; unserializable registers in the current session!
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))))

;;;;; Saveplace (persist locations in buffer)

(use-package saveplace
  :hook (zy-first-file . save-place-mode)
  :config
  (defadvice! zy--dont-prettify-saveplace-cache-a (fn)
    "`save-place-alist-to-file' uses `pp' to prettify the contents of its cache.
`pp' can be expensive for longer lists, and there's no reason to
prettify cache files, so this replace calls to `pp' with the much
faster `prin1'."
    :around #'save-place-alist-to-file
    (letf! ((#'pp #'prin1)) (funcall fn))))

;;;;; Recentf (record recently opened files)

(use-package recentf
  :defer-incrementally easymenu tree-widget timer
  :commands recentf-open-files
  :init
  ;; Enable `recentf-mode' before the first file visit or the first call to
  ;; `recentf-open-files'.
  (add-hook! zy-first-file (recentf-mode 1))
  (add-transient-hook! `recentf-open-files (recentf-mode 1))

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

  (add-hook! 'dired-mode-hook
    (defun zy--recentf-add-dired-directory-h ()
      "Add dired directories to recentf file list."
      (recentf-add-file default-directory)))

  ;; Clean up recent files when quitting Emacs.
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

;;;; User interface

;; This sections concentrates on improving the user interface of GNU Emacs,
;; either graphical or terminal.

;;;;; Theme Emacs with Modus themes

;; Protesilaus Stavrou, the author of Modus themes, is a really cool bro.

(use-package modus-themes
  ;; Modus themes are built-in now, but I prefer using the latest version for a
  ;; greater feature set.
  :straight t
  :demand t
  :config
  (setq!
   modus-themes-italic-constructs t
   modus-themes-bold-constructs t
   ;; Headings are not sized for Modus Themes shipped with Emacs 28
   ;; Maybe I should use the non-built-in version instead
   modus-themes-headings '((0 . (rainbow background 1.3))
			   (1 . (rainbow background overline 1.5))
			   (2 . (rainbow background overline 1.4))
			   (3 . (rainbow background overline 1.3))
			   (4 . (rainbow background overline 1.2))
			   (5 . (rainbow background overline 1.1))
			   (t . (rainbow background overline 1.0)))
   modus-themes-hl-line '(intense)
   modus-themes-markup '(background intense)
   modus-themes-mixed-fonts t
   modus-themes-region '(accented no-extend)
   modus-themes-org-blocks '(gray-background)
   modus-themes-prompts '(background))
  (load-theme 'modus-vivendi 'no-confirm))

;;;;; Mode line

;;;;;; Column and line number

(use-package position-in-buffer
  :defer t
  :init
  ;; Let column number be based on one.
  (setq! column-number-indicator-zero-based nil)
  ;; Display column and line number like this.
  (setq! mode-line-position-column-line-format '(" %l:%c"))
  ;; Enable column number display in the mode line.
  (column-number-mode 1))

;;;;; Hl-line (highlight the current line)

;; TODO: Replace this with Pulsar, a fantastic package by Protesilaus Stavrou

(use-package hl-line
  :hook (zy-first-buffer . global-hl-line-mode)
  :config
  ;; Only highlight line in the current window.
  (setq! global-hl-line-sticky-flag nil))

;;;;; Rainbow colored delimiters

;; Hook `rainbow-delimiters-mode' to specific language modes.

(use-package rainbow-delimiters
  :straight t
  :commands (rainbow-delimiters-mode))

;;;;; Line numbers

;; Line numbers display.
(use-package display-line-numbers
  :defer t
  :init
  ;; Explicitly define a width to reduce the cost of on-the-fly computation.
  (setq-default display-line-numbers-width 3)

  ;; Show absolute line numbers for narrowed regions to make it easier to tell
  ;; the buffer is narrowed, and where you are, exactly.
  (setq-default display-line-numbers-widen t)

  ;; Enable line numbers for these modes only.
  (add-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode))

;;;;; Fonts for various faces

(use-package setup-font
  :defer t
  :init
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

  (zy-maybe-setup-font-faces))


;;;;; No ringing the bell

;; Everytime I type C-g to stop something, it rings the bell.  This is normally
;; not a big deal, but I really hate the bell sound on Windows!  So instead of
;; ringing the bell, I make Emacs flash the mode line instead.

(defun zy-flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq ring-bell-function #'zy-flash-mode-line)

;;;; Features

;; This section is for settings that provide additional features for Emacs.

;;;;; Orderless completion style

(use-package orderless
  :straight t
  :config
  ;; Custom Orderless dispatchers.  These are adapted from Protesilaus Stavrou's
  ;; configuration.

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

  ;; Initialism matching.
  (defun zy-orderless-initialism-dispatcher (pattern _index _total)
    "Initialism dispatcher using the comma suffix."
    (when (string-suffix-p "," pattern)
      `(orderless-initialism . ,(substring pattern 0 -1))))

  ;; Flex matching.
  (defun zy-orderless-flex-dispatcher (pattern _index _total)
    "Flex dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  ;; Apply matching styles and dispatchers.
  (setq! orderless-matching-styles '(orderless-literal
				     orderless-prefixes
				     orderless-flex
				     orderless-regexp)
	 orderless-style-dispatchers '(zy-orderless-literal-dispatcher
				       zy-orderless-no-literal-dispatcher
				       zy-orderless-initialism-dispatcher
				       zy-orderless-flex-dispatcher)))

;;;;; Vertico and Marginalia (minibuffer enhancements)

(use-package vertico
  :straight t
  :init
  (setq!
   ;; Use the orderless completion style.
   completion-styles '(orderless basic)
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

;;;;; Consult (additional completing-read commands)

(use-package consult
  :straight t
  :commands (consult-completion-in-region
	     consult-recent-file)
  :general
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
   "l" 'consult-line)
  :init
  ;; Perform `completion-in-region' with Consult.
  (setq! completion-in-region-function 'consult-completion-in-region)

  ;; Little hack: Enable `recentf-mode' the first time `consult-recent-file' is
  ;; called, without advising the function itself, as that would break
  ;; `consult-customize'.
  (defun zy--first-consult-recent-file ()
    "Transient replacement for `consult-recent-file'.

This function enables `recentf-mode', remaps the keybinding of
itself to `consult-recent-file', can finally call
`consult-recent-file'."
    (interactive)
    (recentf-mode 1)
    (general-def [remap zy--first-consult-recent-file] 'consult-recent-file)
    (consult-recent-file))
  (general-def
    :keymaps 'ctl-x-map
    "R" #'zy--first-consult-recent-file)

  :config
  (consult-customize consult-recent-file
		     :preview-key (kbd "M-.")))


;;;;; Embark (at-point dispatcher)

(use-package embark
  :straight t
  :general
  ("M-m" 'embark-act
   "M-z" 'embark-dwim))

;; Embark and Consult integration.
(use-package embark-consult
  :straight t
  :after (embark consult))


;;;;; Version control (Magit)

;; Currently I only use Git for version control, so the built-in `vc' feature is
;; disabled.
(use-package vc-hooks
  :init
  ;; Do not handle any backends.
  (setq! vc-handled-backends nil))

;; Use Magit as the Git interface.
(use-package magit
  :straight t
  :defer-incrementally
  (dash f s with-editor git-commit package eieio transient)
  :general
  (:keymaps 'ctl-x-map
   "v" 'magit-status
   "C-v" 'magit-dispatch
   "M-v" 'magit-file-dispatch)
  :init
  (setq!
   ;; Do not use default Magit key bindings.
   magit-define-global-key-bindings nil
   ;; Show commit time in the status buffer.
   magit-status-margin '(t age magit-log-margin-width nil 18)))

;;;;; Corfu (completion at point)

(use-package corfu
  :straight '(corfu :files (:defaults "extensions"))
  ;; Enable auto completion for almost every modes.
  :hook (prog-mode text-mode conf-mode)
  :config
  (setq!
   ;; Make completion automatically.
   corfu-auto t
   ;; Make completion a bit more aggresive.
   corfu-auto-prefix 2
   ;; On-the-fly completion.
   corfu-auto-delay 0))

;; Corfu extensions.

;; Define the load path at compile time.
(eval-and-compile
  (defun zy--corfu-extensions-load-path ()
    (file-name-concat straight-base-dir
		      "straight"
		      straight-build-dir
		      "corfu/extensions")))

(use-package corfu-indexed
  :load-path (lambda () (zy--corfu-extensions-load-path))
  :hook corfu-mode)

(use-package corfu-info
  :load-path (lambda () (zy--corfu-extensions-load-path)))

;; Enable Corfu in Terminal via Corfu-terminal

(use-package corfu-terminal
  :straight t
  :after corfu
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode 1))

;;;;; Cape provides more completion-at-point functions

(use-package cape
  :straight t
  :commands (cape-file)
  :init
  ;; Complete file names where Corfu is enabled.
  (add-hook! corfu-mode
    (add-to-list 'completion-at-point-functions #'cape-file)))

;;;; File type specific settings

;; This section enhances Emacs on specific file types, mostly programming
;; languages.

;;;;; Emacs Lisp

(autoload 'zy-lisp-indent-function "zyutils" nil nil 'function)

(use-package elisp-mode
  :defer t
  :init
  (add-hook! 'emacs-lisp-mode-hook
    'outline-minor-mode
    'rainbow-delimiters-mode)
  :config
  (setq-hook! 'emacs-lisp-mode-hook
	      ;; Don't treat autoloads or sexp openers as outline headers.  Use
	      ;; hideshow for that.
	      outline-regexp "[ \t]*;;;;*[^ \t\n]")

  ;; Proper indent function.
  (advice-add #'lisp-indent-function :override 'zy-lisp-indent-function))

;;;; The end

(provide 'init)
;;; init.el ends here
