;;; init-bench.el --- Benchmarking startup -*- lexical-binding: t -*-

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

;; Measure the time took to load each part of the configuration.

;;; Code:

(require 'cl-lib)


;; Benchmarking utility

(defvar before-bench-time (current-time))

(defvar zy/bench-timetable nil
  "A list of benchmark results.

Each element should be a list like (SINCE FEATURE TAKEN).  SINCE
is the time since `before-init-time' before loading feature
FEATURE, and TAKEN is the time taken to load FEATURE.")

(cl-eval-when 'compile
  (defmacro zy/load-and-benchmark
      (file feature-name &optional noerror nomessage nosuffix must-suffix)
    "Like `load', but benchmark the loading.

The loading of the file will be benchmarked by ZyEmacs.
FEATURE-NAME is the FEATURE used in `zy/bench-timetable'.

Other arguments correspond to those of `load'."
    `(let ((before-load-time (current-time)))
       (load ,file ,noerror ,nomessage ,nosuffix ,must-suffix)
       (push (list
	      (time-subtract before-load-time before-init-time)
	      ,feature-name
	      (time-since before-load-time))
	     zy/bench-timetable))))


;; Function to compile, load and benchmark a ZyEmacs feature

(defun zy/-require-compiled-version (zyemacs-feature)
  "Require the compiled version of ZYEMACS-FEATURE.

ZYEMACS-FEATURE is a feature of ZyEmacs config, which must be inside
the \"lisp\" directory under `user-emacs-directory'.

This function loads features faster than `require' or 'load',
because it omits the searching inside `load-path'."
  (unless (featurep zyemacs-feature)
    (let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
	   (basename (symbol-name zyemacs-feature))
	   (el (expand-file-name (concat basename ".el") lisp-dir))
	   (elc (expand-file-name (concat basename ".elc") lisp-dir)))
      ;; Try to compile the updated feature
      (condition-case-unless-debug nil
	  (when (or (not (file-exists-p elc))
		    (and (file-writable-p elc)
			 (file-newer-than-file-p el elc)))
	    (message "Recompiling %s..." el)
	    (let ((after-change-major-mode-hook nil)
		  (prog-mode-hook nil)
		  (emacs-lisp-mode-hook nil)
		  (byte-compile-verbose nil))
	      (byte-compile-file el))
	    (message "Recompiling %s...done" el))
	(error
	 (message "Recompiling %s...failed" el)
	 (when (file-exists-p elc)
	   (delete-file elc))))
      ;; Load and benchmark the feature
      (zy/load-and-benchmark el zyemacs-feature nil 'nomessage))))

(cl-eval-when 'compile
  (defmacro zy/require (zyemacs-feature)
    "Require the compiled version of ZYEMACS-FEATURE.

This is a wrapper for `zy/-require-compiled-version'."
    `(zy/-require-compiled-version ,zyemacs-feature)))


;; Display the startup time at startup

(defun display-startup-echo-area-message ()
  "Replace the default startup message."
  (message
   (format "ZyEmacs ready in %.2f seconds."
           (float-time
            (time-subtract after-init-time before-init-time)))))


;; Benchmark this file

(push `(,(time-subtract before-bench-time before-init-time)
	init-bench
	,(time-since before-bench-time))
      zy/bench-timetable)
(makunbound 'before-bench-time)


(provide 'init-bench)

;;; init-bench.el ends here.
