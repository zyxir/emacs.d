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

;;;; Version Check

;; Check version at both compile and runtime.
(eval-and-compile
  ;; The minimum version to run this configuration is 28.1.
  (when (< emacs-major-version 28)
    (user-error
     "Emacs version is %s, but this config requires 28.1 or newer"
     emacs-version)))

;;;; Startup Hacks

;; Some hacks that make startup faster.  Most of them are learnt from Doom
;; Emacs, so thank you Henrik Lissner!

;;;;; Garbage Collection

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

;;;;; File Name Handlers

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

;;;;; GUI Noises

;; Reduce some GUI noises can also reduce startup time.

(unless noninteractive
  ;; Frame resizing caused by font changing can increase startup time
  ;; dramatically.  Setting this value to t stops that from happening.
  (setq frame-inhibit-implied-resize t)

  ;; Do not show any startup screen or message.
  (setq inhibit-startup-screen t)

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

;;; Helper Functions

;; Sugar functions and macros.  Many of them are directly copied or adapted from
;; Doom Emacs.

;;;; Logging

(defun zy--log (text &rest args)
  "Log a message TEXT formatted with ARGS.

The current time till `before-init-time' is prefixed to the
output."
  (message "%s - %s"
	   (float-time (time-subtract (current-time) before-init-time))
	   (propertize (format text args) 'face 'font-lock-doc-face)))

(defmacro zy-log (message &rest args)
  "Log MESSAGE formatted with ARGS in *Messages*.

If `init-file-debug' is nil, do nothing."
  (declare (debug t))
  `(when init-file-debug (zy--log ,message ,@args)))

;;;; Symbol Handling

(defun zy-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun zy-keyword-intern (str)
  "Convert STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

;;;; Hook Functions

(define-error 'hook-error "Error in a hook.")

(defvar zy--hook nil
  "Currently triggered hook.")

(defun zy-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.

Should be used with `run-hook-wrapped'."
  (zy-log "hook:%s: run %s" (or zy--hook '*) hook)
  (condition-case-unless-debug e
      (funcall hook)
    (error
     (signal 'hook-error (list hook e)))))

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

;;;; Lisp Sugars

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

(defmacro setq! (&rest settings)
  "Setting variables according to SETTINGS.

Unlike `setq', this triggers custom setters on variables.  Unlike
`setopt', this won't needlessly pull in dependencies."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set)
				  #'set-default-toplevel-value)
                              ',var ,val))))

;; TODO: Implement this like Doom Emacs does.
(defmacro add-hook! (hooks &rest rest)
  "A extended version of `add-hook'.")

;; TODO: Implement this like Doom Emacs does.
(defmacro setq-hook! (hooks &rest rest)
  "Set buffer-local variables on HOOKS.")

;;; Globals

;;;; Zyxir's Definitions

(defgroup zyxir nil
  "Zyxir's customization layer over Emacs."
  :group 'emacs)

;;;; Custom Hooks

;;;;; Switch Buffer Hook

(defcustom zy-switch-buffer-hook nil
  "Hooks run after changing the current buffer."
  :type 'hook
  :group 'zyxir)

(defun zy-run-switch-buffer-hooks-h (&rest _)
  "Run all hooks in `zy-switch-buffer-hook'."
  (let ((gc-cons-threshold most-positive-fixnum)
	(inhibit-redisplay t))
    (run-hooks 'zy-switch-buffer-hook)))

(add-hook 'window-buffer-change-function #'zy-run-switch-buffer-hooks-h)

;;;;; First Buffer Hook

(defcustom zy-first-buffer-hook nil
  "Hooks run before the first interactively opened buffer."
  :type 'hook
  :local 'permenant-local
  :group 'zyxir)

(zy-run-hook-on 'zy-first-buffer-hook
		'(find-file-hook zy-switch-buffer-hook))

;;;;; First File Hook

(defcustom zy-first-file-hook nil
  "Hooks run before the first interactively opened file."
  :type 'hook
  :local 'permenant-local
  :group 'zyxir)

(zy-run-hook-on 'zy-first-file-hook
		'(find-file-hook dired-initial-position-hook))

(provide 'init)
;;; init.el ends here
