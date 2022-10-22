;;; init-load.el --- Code loading -*- lexical-binding: t -*-


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

;; This file bootstraps Straight as the package manager, and introduces and
;; advanced loading system to ZyEmacs.

;;; Code:

;;;; Bootstrap Straight

(setq-default
 ;; Cache autoloads into a single file to speed up startup
 straight-cache-autoloads t
 ;; Do not check for modifications at startup, as I don't modify 3rd-party
 ;; packages very often
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


;;;; Debug Logger

;; The debug logger only works when Emacs is launched with "--debug-init"

(defalias 'zy/log
  (if (and (boundp 'init-file-debug) init-file-debug)
      (lambda (module format-string &rest args)
	"Display a ZyEmacs log message.

MODULE is the module name to show.  FORMAT-STRING is the format
control string, and ARGS are data to be formatted under control
of the string."
	(let* ((content
		(format "[%s %s] - %s - %s\n"
			(propertize "ZyEmacs"
				    'face 'font-lock-variable-name-face)
			(propertize (if (stringp module)
					module
				      (symbol-name module))
				    'face 'font-lock-keyword-face)
			(propertize (format-time-string "%FT%T")
				    'face 'font-lock-constant-face)
			(propertize (apply 'format format-string args)
				    'face 'font-lock-doc-face)))
	       (buffer (get-buffer-create "*ZyEmacs Log*"
					  'inihibit-buffer-hooks))
	       (inhibit-read-only t))
	  (with-current-buffer buffer
	    (unless buffer-read-only
	      (read-only-mode 1))
	    (goto-char (point-max))
	    (insert content))))
    (lambda (&rest _))))


;;;; Snippet Features

;; In ZyEmacs, a snippet feature is a special type of feature.  Usually, a
;; feature is provided by a file with the same name, but in ZyEmacs, a snippet
;; is not provided by any file, instead it has its function definition as its
;; actual content, and has a `snipp' property to indicate that it is a snippet
;; feature.

;; With this mechanism introduced, you could require a feature with
;; `zy/require', which loads its function definition if it is a snippet feature,
;; or loads the file if it is not.

(defmacro zy/defsnip (feature &rest body)
  "Define FEATURE as a snippet feature.

FEATURE should be a symbol, and BODY is the content of its
function definition.  A `provide' statement is always appended to
BODY so that it will not be run twice."
  (declare (indent 1))
  `(prog1
       (defalias ,feature
	 (lambda nil ,@body (provide ,feature)))
     (put ,feature 'snipp t)))

(defun zy/load-feature (feature)
  "Load feature FEATURE in a proper way.

if FEATURE is a snippet feature (if it has a `snipp' property),
call its function definition, otherwise call `require' on the
feature.

`zy/load-feature' is set to t while loading FEATURE."
  (if (get feature 'snipp)
      (funcall feature)
    (require feature)))

(when init-file-debug
  (defun zy/time-around-load-feature (oldfun feature)
    "Wrapper of `zy/load-feature'.

Run OLDFUN on FEATURE, but time the run, and print time taken."
    (let ((start-time (current-time)))
      (prog1 (funcall oldfun feature)
	(zy/log 'snip "Requiring %s took %.1f milliseconds"
		feature
		(* 1000 (float-time (time-since start-time)))))))
  (advice-add 'zy/load-feature :around 'zy/time-around-load-feature))

(defun zy/require (feature)
  "Try to require feature FEATURE.

If FEATURE is already loaded, return itself.  Otherwise call
`zy/load-feature' on it.

If an error is encountered during calling a function definition
or requiring a feature, this function regains control and report
the error."
  (if (featurep feature)
      feature
    (condition-case-unless-debug err
	(zy/load-feature feature)
      (error
       (message "Error encountered while requiring %s:" feature)
       (pp err)
       nil)
      (t nil))))


;;;; Lazy Loader (lload)

;; Lazy loader extends the classic lazy loader based on `eval-after-load'.

(defmacro zy/lload-register (feature &rest features)
  "Load FEATURE once all of FEATURES are loaded.

FEATURE and FEATURES are feature symbols."
  (let ((result-sexp `(zy/require ,feature)))
    (mapc (lambda (feat)
	    (setq result-sexp
		  `(with-eval-after-load ,feat ,result-sexp)))
	  features)
    result-sexp))


;;;; Event-Driven Loader (edload)

;; Event-driven loader delay the loading of a feature till a set of events.

;; Event `pre-command': triggered by `pre-command-hook'

(defvar zy/edload-pre-command-queue nil
  "Features bound to the `pre-command' event.")

(add-hook 'pre-command-hook
	  (lambda ()
	    (when zy/edload-pre-command-queue
	      (zy/log 'edload "`pre-command' triggered"))
	    (mapc #'zy/require zy/edload-pre-command-queue)
	    (setq zy/edload-pre-command-queue nil)))

;; Event `post-command': triggered by `post-command-hook'

(defvar zy/edload-post-command-queue nil
  "Features bound to the `post-command' event.")

(add-hook 'post-command-hook
	  (lambda ()
	    (when zy/edload-post-command-queue
	      (zy/log 'edload "`post-command' triggered"))
	    (mapc #'zy/require zy/edload-post-command-queue)
	    (setq zy/edload-post-command-queue nil)))

;; Event `text-mode': triggered by `text-mode-hook'

(defvar zy/edload-text-mode-queue nil
  "Features bound to the `text-mode' event.")

(add-hook 'text-mode-hook
	  (lambda ()
	    (when zy/edload-text-mode-queue
	      (zy/log 'edload "`text-mode' triggered"))
	    (mapc #'zy/require zy/edload-text-mode-queue)
	    (setq zy/edload-text-mode-queue nil)))

;; Event `prog-mode': triggered by `prog-mode-hook'

(defvar zy/edload-prog-mode-queue nil
  "Features bound to the `prog-mode' event.")

(add-hook 'prog-mode-hook
	  (lambda ()
	    (when zy/edload-prog-mode-queue
	      (zy/log 'edload "`prog-mode' triggered"))
	    (mapc #'zy/require zy/edload-prog-mode-queue)
	    (setq zy/edload-prog-mode-queue nil)))

;; Event registering

(defmacro zy/edload-register (feature &rest events)
  "When one of EVENTS occur, load FEATURE.

FEATURE is a feature symbol, and all of EVENTS are event symbols."
  (let ((result-sexp '()))
    (mapc (lambda (event)
	    (when (equal event '(quote pre-command))
	      (push `(push ,feature zy/edload-pre-command-queue) result-sexp))
	    (when (equal event '(quote post-command))
	      (push `(push ,feature zy/edload-post-command-queue) result-sexp))
	    (when (equal event '(quote find-file))
	      (push `(push ,feature zy/edload-find-file-queue) result-sexp))
	    (when (equal event '(quote prog-mode))
	      (push `(push ,feature zy/edload-prog-mode-queue) result-sexp)))
	  events)
    (if (cdr result-sexp)
	(cons 'progn result-sexp)
      (car result-sexp))))


;;;; Incremental Loader (incload)

;; Incremental loader loads a queue of features incrementally with a
;; configurable interval.

;;;;; Constants and Variables

(defconst zy/incload-idle 2.0
  "Normal load interval for the incremental loader.")

(defconst zy/incload-lag 0.3
  "Lag threshold for the incremental loader.")

(defvar zy/incload-rescheduled-timer nil
  "The rescheduled idle timer.")

(defvar zy/incload-rescheduled-idle zy/incload-idle
  "The idle time for the rescheduled timer.")

(defvar zy/incload-queue nil
  "The queue of loading unit for the incremental loader.

Each loading unit in the queue is a list of the form:

  (FEATURE HEAVYP WEIGHT)

FEATURE is the feature to be loaded, be it a snippet
feature or not.

HEAVYP indicates if feature takes a lot of time to load.  This
will affect of loading strategy in runtime.

WEIGHT is the weight of this loading unit.  It affects loading
priority.")

(defvar zy/incload-queue-initial nil
  "Initial value of `zy/incload-queue'.")

(defvar zy/incload-feature-queue nil
  "A queue of FEATUREs of each element in `zy/incload-queue'.")

;;;;; Queue Management

(defun zy/incload-normalize-unit (unit)
  "Return the normalized version of loading unit UNIT.

UNIT is normalized as required by `zy/incload-register'."
  (if (symbolp unit)
      `(,unit nil 0)
    (list (car unit)
	  (cadr unit)
	  (if (caddr unit) (caddr unit) 0))))

(defun zy/incload-unit-in-queue-p (unit)
  "Return t if loading unit UNIT is already in queue."
  (if (symbolp unit)
      (memq unit zy/incload-feature-queue)
    (memq (car unit) zy/incload-feature-queue)))

(defun zy/incload-register-unit (unit)
  "Add loading unit UNIT to the queue if it is not already there."
  (unless (zy/incload-unit-in-queue-p unit)
    (setq unit (zy/incload-normalize-unit unit))
    (push unit zy/incload-queue)
    (push (car unit) zy/incload-feature-queue)))

;;;;; Loading Mechanism

(defun zy/incload-load (&optional rescheduled)
  "Load some units from `zy/incload-queue'.

If optional argument RESCHEDULED is nil or omitted, which means
this is not a rescheduled run, then the rescheduled timer and
rescheduled idle time will be cleared.

Pop loading units out of `zy/incload-queue' and load them.  At
least load one loading unit, and stop loading when:

- There is nothing in `zy/incload-queue' to load.

- The current elapsed time exceeds `zy/incload-lag-normal'.

- The next loading unit has a non-nil value of HEAVYP.

After loading stoppes, reschedule a new timer for the next load."
  ;; Clear rescheduled timer and idle time if necessary
  (unless rescheduled
    (when zy/incload-rescheduled-timer
      (cancel-timer zy/incload-rescheduled-timer))
    (setq zy/incload-rescheduled-idle zy/incload-idle))
  (when zy/incload-queue
    ;; Load only if there is something in the queue
    (zy/log 'incload "---- Incload starts loading units ----")
    (let ((start-time (current-time))
	  (elapsed-time 0)
	  next-heavyp)
      ;; Load untill any of the three conditionals returns nil
      (while (and zy/incload-queue
		  (< elapsed-time zy/incload-lag)
		  (not next-heavyp))
	(zy/require (car (pop zy/incload-queue)))
	(setq elapsed-time (float-time (time-since start-time))
	      next-heavyp (cadr (car zy/incload-queue)))))
    (zy/log 'incload "---- Incload stops loading units ----"))
  ;; Prepare the next load
  (setq zy/incload-rescheduled-idle (+ zy/incload-idle
				       zy/incload-rescheduled-idle)
	zy/incload-rescheduled-timer
	(run-with-idle-timer zy/incload-rescheduled-idle nil
			     'zy/incload-load 'rescheduled)))

(defun zy/incload-weight< (unit1 unit2)
  "Return t if UNIT1 has a greater WEIGHT than UNIT2."
  (< (caddr unit1) (caddr unit2)))

(defun zy/incload-init ()
  "Initialize the incremental loader.

Sort `zy/incload-queue' according to WEIGHT of each loading unit,
copying its value to `zy/incload-queue-initial', and create the
first idle timer for `zy/incload-load'."
  (setq zy/incload-queue-initial
	(reverse (sort zy/incload-queue 'zy/incload-weight<))
	zy/incload-queue zy/incload-queue-initial)
  (run-with-idle-timer zy/incload-idle 'repeat 'zy/incload-load))

(add-hook 'emacs-startup-hook 'zy/incload-init)

;;;;; The Interface Macro

(defmacro zy/incload-register (&rest units)
  "Register loading units UNITS for incremental loading.

The incremental loader loads units in the front first.  So a
former unit should possibly be the dependency of a later unit, to
maintain the incrementality of the loading process.

Each loading unit in UNITS is a list of the form:

  (FEATURE [HEAVYP [WEIGHT]])

FEATURE is the feature to be loaded, be it a snippet
feature or not.

HEAVYP indicates if feature takes a lot of time to load.  This
will affect of loading strategy in runtime.  HEAVYP is nil by
default.

WEIGHT is the weight of the loading unit.  It indicates the
priority of the unit in the loading queue.  WEIGHT is 0 by
default.

You can also use a single FEATURE symbol as a loading unit, and
it will be converted to a proper list."
  (let ((result-sexp '()))
    (mapc (lambda (unit)
	    (push `(zy/incload-register-unit ,unit) result-sexp))
	  units)
    (if (cdr result-sexp)
	(cons 'progn (reverse result-sexp))
      (car result-sexp))))


(provide 'init-load)

;;; init-load.el ends here
