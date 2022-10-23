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

(require 'init-benchmark)

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
       (zy/log 'snip "Error encountered while requiring %s:" feature)
       (zy/log 'snip (pp err))
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

;;;;; The Interface

;; The incremental loader manages a queue of features to be loaded
;; incrementally.  Loading of a feature happens when Emacs is idle for a
;; specific period of time.  If Emacs is idle for longer times, the timer is
;; rescheduled to load even more features.

;; The incremental loader also manages the loading sequence of features by some
;; additional properties.  These properties are stored in the property list of
;; the feature symbol.

(defconst zy/incload-idle 1.0
  "Load interval for the incremental loader.")

(defconst zy/incload-lag 0.2
  "Lag threshold for the incremental loader.")

(defvar zy/incload-timer nil
  "The repeated idle timer.")

(defvar zy/incload-rescheduled-timer nil
  "The rescheduled idle timer.")

(defvar zy/incload-reschedule-level 1
  "The current idle time multiplier.")

(defvar zy/incload-queue nil
  "The queue of features for the incremental loader.")

(defvar zy/incload-queue-initial nil
  "Initial value of `zy/incload-queue'.")

(defmacro zy/incload-register (feature &rest args)
  "Add FEATURE into the incremental loading queue.

ARGS specifies additional information to tune the loading of FEATURE.

ARGS could come in pairs with `:priority' and PRIORITY, which
sets the `incload-priority' property of FEATURE to PRIORITY.  The
`incload-priority' property stores the priority of the feature.
Feature with a higher priority loads earlier.  `incload-priority'
is treated as 0 if not specified.

ARGS could come in pairs with `:level' and LEVEL, which sets the
`incload-level' property of FEATURE to LEVEL.  The
`incload-level' property stores approximately the heavyness of
the feature.  A feature with a higher level is more heavy, thus
consumes much more time to load.  A feature with level LEVEL will
only load after Emacs being idle for LEVEL times of
`zy/incload-idle' seconds.  `incload-level' is treated as 1 if
not specified.

If an `:after' argument is parsed, all remaining arguments is
considered dependencies that should be loaded before FEATURE.
Each dependency is either a feature symbol (quoted or unquoted)
like FEATURE, or an argument list (unquoted) that can be directly
passed to `zy/incload-register', so that dependent feature will
be registered by `zy/incload-register' as well."
  (let ((result-sexp `((unless (memq ,feature zy/incload-queue)
			 (push ,feature zy/incload-queue))))
	(after-sexp '())
	arg)
    ;; Parse ARGS in a loop
    (while (and args)
      (setq arg (pop args))
      (cond
       ((eq arg ':priority)
	(push `(put ,feature 'incload-priority ,(pop args)) result-sexp))
       ((eq arg ':level)
	(push `(put ,feature 'incload-level ,(pop args)) result-sexp))
       ((eq arg ':after)
	(mapc (lambda (arg)
		(when (eq (car arg) 'quote)
		  (setq arg (cadr arg)))
		(push (if (symbolp arg)
			  `(zy/incload-register ',arg)
		        `(zy/incload-register ,@arg))
		      after-sexp))
	      args)
	(setq args nil
	      after-sexp (reverse after-sexp)
	      result-sexp (nconc after-sexp result-sexp)))
       (t nil)))
    ;; Format the final sexp
    (if (cdr result-sexp)
	(cons 'progn result-sexp)
      (car result-sexp))))


;;;;; Loading Mechanism

(defun zy/incload-get-level (feature)
  "Get the `incload-level' property of FEATURE.

If FEATURE has no `incload-level' property, return 1."
  (let ((level (get feature 'incload-level)))
    (if level level 1)))

(defun zy/incload-load (&optional rescheduled)
  "Load some features from `zy/incload-queue'.

If optional argument RESCHEDULED is nil or omitted, which means
this is not a rescheduled run, then the rescheduled timer and
rescheduled idle time will be cleared.

Get the next loadable feature from `zy/incload-queue'.  A feature
is loadable if `current-idle-time' is greater than its
`incload-level' property multiplying `zy/incload-idle'.  Continue
loading more loadable features until:

- There is no more loadable features in `zy/incload-queue'.

- The current elapsed time exceeds `zy/incload-lag'.

After loading stoppes, reschedule a new timer for the next load."
  ;; Clear rescheduled timer and idle time if necessary
  (unless rescheduled
    (when zy/incload-rescheduled-timer
      (cancel-timer zy/incload-rescheduled-timer))
    (setq zy/incload-reschedule-level 1))
  ;; Load only if there is something in the queue
  (when zy/incload-queue
    (zy/log 'incload "\\\\ Loading starts with reschedule level %d"
	    zy/incload-reschedule-level)
    (let ((start-time (current-time))
	  (elapsed-time 0)
	  (curpos zy/incload-queue)
	  (curind 0))
      ;; Find and load loadable features in a loop
      (while (and curpos (< elapsed-time zy/incload-lag))
	(if (<= (zy/incload-get-level (car curpos))
		zy/incload-reschedule-level)
	    (progn
	      ;; Load the feature
	      (zy/require (car curpos))
	      ;; Exclude the feature from the queue
	      (setf elapsed-time (float-time (time-since start-time))
		    curpos (cdr curpos)
		    (nthcdr curind zy/incload-queue) curpos))
	  ;; Go to the next position of the queue
	  (setq curpos (cdr curpos)
		curind (1+ curind)))))
    (zy/log 'incload "// Loading stops with reschedule level %d"
	    zy/incload-reschedule-level))
  ;; Prepare the rescheduled timer
  (setq zy/incload-reschedule-level (1+ zy/incload-reschedule-level)
	zy/incload-rescheduled-timer
	(run-with-idle-timer (* zy/incload-reschedule-level
				zy/incload-idle)
			     nil
			     'zy/incload-load 'rescheduled)))

(defun zy/incload-setup-timer ()
  "Setup the repeated timer for incload."
  (setq zy/incload-timer
	(run-with-idle-timer zy/incload-idle 'repeat 'zy/incload-load)))

(defun zy/incload-priority< (feature1 feature2)
  "Return t if FEATURE1 has a greater PRIORITY than FEATURE2."
  (let ((priority1 (get feature1 'incload-priority))
	(priority2 (get feature2 'incload-priority)))
    (< (if priority1 priority1 0) (if priority2 priority2 0))))

(defun zy/incload-init ()
  "Initialize the incremental loader.

Sort `zy/incload-queue' according to the `incload-priority'
property of each feature, copying its value to
`zy/incload-queue-initial', and create the first idle timer for
`zy/incload-load'."
  (setq zy/incload-queue-initial
	(reverse (sort zy/incload-queue 'zy/incload-priority<))
	zy/incload-queue zy/incload-queue-initial)
  ;; Setup the first timer with a longer idle time, as Emacs is idle during
  ;; startup
  (run-with-idle-timer (+ zy/incload-idle
			  (float-time
			   (time-subtract after-init-time
					  before-init-time)))
		       'repeat 'zy/incload-setup-timer))

(add-hook 'emacs-startup-hook 'zy/incload-init)


(provide 'init-load)

;;; init-load.el ends here
