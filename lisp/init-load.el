;;; init-load.el --- Configuration code loading -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

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

;; 1. Manage the loading (autoloading) of packages (as Borg drones).
;; 2. Provide snippet management, event-driven loader and incremental loader.
;; 3. Provide several useful macros for configuration writting.

;;; Code:

(require 'cl-lib)


;; Autoload Borg functions

(autoload 'borg-assimilate "borg" nil 'interactive)
(autoload 'borg-build "borg" nil 'interactive)
(autoload 'borg-clone "borg" nil 'interactive)
(autoload 'borg-remove "borg" nil 'interactive)
(with-eval-after-load 'magit (require 'borg))


;; Ensure the loaddefs file

(defvar zy/loaddefs-file
  (expand-file-name "lisp/init-loaddefs.el" user-emacs-directory)
  "The loaddefs file of ZyEmacs.")

(add-to-list 'load-path (expand-file-name "lib/zyutils" user-emacs-directory))
(autoload 'zy-mngt/collect-loaddefs "zy-mngt")

(defun zy/ensure-loaddefs-file (&optional must-regen)
  "Return the up-to-date loaddefs file.

The loaddefs file is considered up-to-date if it is not older
than the .gitmodules file.  Normally this function only
re-generate the loaddefs file if it is not up-to-date or does not
exist.  If optional argument MUST-REGEN is non-nil, however, it
always re-generate the loaddefs file."
  (let* ((gitmodules-file
	  (expand-file-name ".gitmodules" user-emacs-directory)))
    ;; Re-generate the loaddefs file if necessary
    (when (or must-regen
	      (not (file-exists-p zy/loaddefs-file))
	      (file-newer-than-file-p gitmodules-file zy/loaddefs-file))
      (zy-mngt/collect-loaddefs zy/loaddefs-file))
    ;; Return the path of the loaddefs file
    zy/loaddefs-file))

(defun zy/regen-loaddefs ()
  "Regenerate and load the loaddefs file."
  (interactive)
  (load (zy/ensure-loaddefs-file 'must-regen)))

(zy/ensure-loaddefs-file)


;; Snippet management

;; In ZyEmacs, a snippet is a special type of function that takes no arguments,
;; and provide itself as a feature if executed.  A snippet stores its additional
;; information in ints property list.

;; A feature snippet is just a feature symbol, with additonal information stored
;; in its property list as well, so that it can be treated just like a snippet.

;; A snippet is like a short Emacs Lisp file, except that it cannot be load with
;; `require' or `load'.  However, this provides opportunity for applying
;; advanced loading techniques to manage them.

(cl-defmacro zy/defsnip (name (&key (dependencies nil)
				    (lazyload nil)
				    (events nil)
				    (weight nil))
			      &optional docstring &rest body)
  "Define NAME as a snippet.

NAME is the name of the snippet, and should not be quoted.

Keyword argument DEPENDENCIES is a list of dependent features or
snippets that should be loaded before NAME is load.  Make sure to
put lower-level dependencies in the former place of the list.

If keyword argument LAZYLOAD is a symbol or a list of symbols,
lazy load the snippet untill all the symbols (features) are
loaded.

Keyword argument EVENTS is a list of events at which the ZyEmacs
event-driven loader should load the snippet.

If keyword argument WEIGHT is a number, the ZyEmacs incremental
loader will register it with priority INCLOAD.

Optional argument DOCSTRING is the docstring used to describe
NAME as a variable.

BODY is the function body of the snippet.

Return the snippet as a symbol."
  (declare (doc-string 3) (indent 2))
  (when (and docstring (not (stringp docstring)))
    (push docstring body)
    (setq docstring nil))
  `(prog1
       ;; Wrap BODY in a feature management block
       (defun ,name ()
	 ,docstring
	 ,@body
	 (provide ',name))
     (function-put ',name 'snip 'function)
     (function-put ',name 'snip-dependencies ,dependencies)
     ,(when lazyload
	`(zy/lazyload--register ',name ,lazyload))
     ,(when events
	`(zy/edload--register ',name ,events))
     ,(when (numberp weight)
	`(push (cons ',name ,weight) zy/incload--weight-alist))))

(cl-defun zy/snip-from-feature (feature &key
					(dependencies nil)
					(events nil)
					(weight nil))
  "Define a feature snippet from FEATURE.

FEATURE is a quoted feature name.  The created snippet use
FEATURE as its name, but has no function difinition.

Optional keyword arguments DEPENDENCIES, EVENTS and WEIGHT are
the same as in `zy/defsnip'.

Return FEATURE."
  (declare (indent defun))
  (when (symbolp feature)
    (function-put feature 'snip 'feature)
    (function-put feature 'snip-dependencies dependencies)
    (when events
      (zy/edload--register feature events))
    (when (and (numberp weight) (boundp 'zy/incload--weight-alist))
      (push (cons feature weight) zy/incload--weight-alist))
    feature))

(defmacro zy/snipp (symbol)
  "Return t if SYMBOL is a snip."
  `(function-get ,symbol 'snip))

(defun zy/ensure-snip (feature)
  "Ensure that FEATURE is a feature snippet.

If it is already a feature snippet, return FEATURE.  Otherwise
use `zy/snip-from-feature' on it and return the result."
  (if (zy/snipp feature)
      feature
    (zy/snip-from-feature feature)))

(defun zy/run-snip (snip)
  "Run snippet SNIP.

If SNIP is already loaded (determined via `featurep'), do nothing
and return nil.

Otherwise, call `funcall' on SNIP if it is a ordinary snippet,
and `require' it if it is a feature snippet.  Return the result
of `funcall' or `require'."
  (unless (featurep snip)
    (if (eq (function-get snip 'snip) 'function)
	(funcall snip)
      (require snip))))


;; Classic lazy loader (lazyload)

;; This is the classic lazy loader based on `eval-after-load', but tweaked so
;; that it works better with snippets.

(defmacro zy/lazyload--register (snip prerequisites)
  "Load snippet SNIP after all PREREQUISITES are loaded."
  (let ((result-sexp `(zy/run-snip ,snip)))
    ;; Normalize PREREQUISITES
    (when (equal (car prerequisites) 'quote)
      (setq prerequisites (cadr prerequisites)))
    (unless (listp prerequisites)
      (setq prerequisites `(,prerequisites)))
    (mapc (lambda (pre)
	    (setq result-sexp
		  `(with-eval-after-load ',pre
		     ,result-sexp)))
	  prerequisites)
    result-sexp))


;; Event-driven loader (edload)

;; In ZyEmacs, the loading of any snip could be postponed till a set of specific
;; events.  This is called the event-driven loader.

;; Event 'pre-command': when `pre-command-hook' is triggered

(defvar zy/edload--pre-command-snips nil
  "Snippets bound to the 'pre-command' event.")

(defun zy/edload--pre-command-run ()
  "Run all snippets in `zy/edload-pre-command-snips'."
  (mapc #'zy/run-snip zy/edload--pre-command-snips)
  (setq zy/edload--pre-command-snips nil))

(add-hook 'pre-command-hook 'zy/edload--pre-command-run)

;; Event 'after-command': when `after-command-hook' is triggered

(defvar zy/edload--after-command-snips nil
  "Snippets bound to the 'after-command' event.")

(defun zy/edload--after-command-run ()
  "Run all snippets in `zy/edload--after-command-snips'."
  (mapc #'zy/run-snip zy/edload--after-command-snips)
  (setq zy/edload--after-command-snips nil))

(add-hook 'after-command-hook 'zy/edload--after-command-run)

;; Event 'find-file': before `after-find-file' is executed

(defvar zy/edload--find-file-snips nil
  "Snippets bound to the 'find-file' event.")

(defun zy/edload--find-file-run (&rest ignored)
  "Run all snippets in `zy/edload--find-file-snips'.

All other arguments IGNORED are ignored."
  (mapc #'zy/run-snip zy/edload--find-file-snips)
  (setq zy/edload--find-file-snips nil))

(advice-add 'after-find-file :before 'zy/edload--find-file-run)

;; Register a snip to a set of events

(defun zy/edload--register (snip events)
  "Register snippet SNIP to events EVENTS.

EVENTS is a list of event symbols.  Even if EVENTS is a single
event symbol, it will be converted to a list containing the event
symbol.

This function should only be used internally."
  (unless (listp events)
    (setq events `(,events)))
  ;; Event 'pre-command'
  (when (memq 'pre-command events)
    (add-to-list 'zy/edload--pre-command-snips snip))
  ;; Event 'find-file'
  (when (memq 'find-file events)
    (add-to-list 'zy/edload--find-file-snips snip)))

(defmacro zy/edload-register (snip &rest events)
  "Register snippet SNIP to events EVENTS.

Return EVENTS if success, or nil otherwise."
  `(when (zy/snipp ,snip)
     (zy/edload--register ,snip ,events)
     ,events))


;; Incremental loader (incload)

;; In ZyEmacs, the incremental loader loads a queue of snippets incrementally.
;; Each time Emacs is idle for a certain time, the loader loads the next snippet
;; in the queue, until everything is fully loaded.

;; The incremental loader can work together with the classic lazy loader (via
;; `autoload' and `eval-after-load'), as well as the ZyEmacs event-driven
;; loader.

(defconst zy/incload--idle 1.0
  "Load next snippet after Emacs is idle for this many time.")

(defconst zy/incload--lag 0.2
  "Each load action takes at least this amount of time.")

(defvar zy/incload--queue nil
  "The queue of snippets waiting to be loaded.

This queue gets shorter as snippets get loaded.  See
`zy/incload-queue' for its initial state.")

(defvar zy/incload-queue nil
  "The queue of snippets that are incrementally loaded.

This queue is copied from `zy/incload--queue' after it is
initialized.")

(defvar zy/incload--weight-alist nil
  "Alist of (SNIP . WEIGHT) values.

SNIP is a snippet, and WEIGHT is its weight.  This alist is for
priotizing the loading of each SNIP, and is used to construct
`zy/incload--queue' during the initialization of the incremental
loader.")

(defun zy/incload--weight< (item1 item2)
  "Return t if ITEM1 has a smaller weight than ITEM2.

Both ITEM1 and ITEM2 are (SNIP . WEIGHT) cons cells."
  (< (cdr item1) (cdr item2)))

(defun zy/incload--load ()
  "Load some snippets from the queue.

Pop one snippet out of `zy/incload--queue'.  Load the snippet if
it is not loaded yet; if already loaded, pop one more out of the
queue and try to load it again.

If the snippet is so short, that loading it took less than
`zy/incload--lag' seconds, then try to load more snippets from
the queue, until the total elapsed time exceeds
`zy/incload--lag'."
  (let ((start-time (current-time))
	(continue-p t)
	snip)
    (while (and zy/incload--queue continue-p)
      ;; Pop one snippet out
      (setq snip (pop zy/incload--queue))
      ;; Skip it if it is already loaded
      (unless (featurep snip)
	;; Run the snippet
	(zy/run-snip snip)
	;; Determine if we have to run more snippet, based on time elapsed
	(setq continue-p
	      (< (float-time (time-since start-time))
		 zy/incload--lag)))))
  ;; Start the next timer if the queue is not empty yet
  (when zy/incload--queue
    (run-with-idle-timer zy/incload--idle nil 'zy/incload--load)))

(defun zy/incload-init ()
  "Initialize the incremental loader.

This function first sorts `zy/incload--weight-alist' by its
WEIGHT value, then pushes each SNIPPET, as well as its
dependencies, into `zy/incload--queue'.  Existing snippet will
not be pushed again."
  (when zy/incload--weight-alist
    (setq zy/incload--weight-alist
	  (sort zy/incload--weight-alist 'zy/incload--weight<))
    ;; Initialize `zy/incload--queue' via `zy/incload--weight-alist'
    (mapc
     (lambda (snip-weight-cons)
       (let* ((snip (car snip-weight-cons))
	      (snip-deps (function-get snip 'snip-dependencies))
	      (snip-deps (if (listp snip-deps) snip-deps `(,snip-deps)))
	      (snip-deps-ensured (mapcar 'zy/ensure-snip snip-deps))
	      (snips (cons snip snip-deps-ensured)))
	 (mapc (lambda (snip)
		 (unless (memq snip zy/incload--queue)
		   (push snip zy/incload--queue)))
	       snips)))
     zy/incload--weight-alist)
    (setq zy/incload-queue zy/incload--queue)
    ;; Start the first timer for `zy/incload--load'
    (run-with-idle-timer zy/incload--idle nil 'zy/incload--load)))

(add-hook 'after-init-hook 'zy/incload-init)


(provide 'init-load)

;;; init-load.el ends here
