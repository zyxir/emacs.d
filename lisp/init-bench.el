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


;; Benchmark `require's

(defvar zy/bench-timetable nil
  "A list of (FEATURE SINCE TAKEN PARENT).

FEATURE is a feature that can be loaded by `require'.  SINCE is
the time since `before-init-time' when the loading of feature
FEATURE begins.  TAKEN is the time taken to load the feature.
PARENT is the parent feature that requires FEATURE explicitly.")

(defvar zy/-bench-feature-stack nil
  "Stack for FEATURE symbols.")

(defun zy/-require-wrapper (oldfun feature &rest args)
  "Wrapper of `require' that save the requiring time.

OLDFUN is the `require' function.  FEATURE is the feature to be
required.  Other arguments ARGS are passed to OLDFUN as well."
  (if (memq feature features)
      ;; Already loaded, skip it
      t
    (let ((before-time (current-time)))
      (push feature zy/-bench-feature-stack)
      ;; Load and benchmark
      (prog1
	  (apply oldfun feature args)
	(pop zy/-bench-feature-stack)
	;; Only benchmark when the feature is successfully loaded
	(when (memq feature features)
	  (push (list feature
		      (time-subtract before-time before-init-time)
		      (time-since before-time)
		      (car zy/-bench-feature-stack))
		zy/bench-timetable))))))

(advice-add 'require :around 'zy/-require-wrapper)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (advice-remove 'require 'zy/-require-wrapper)))


;; Show total startup time after init

(defun display-startup-echo-area-message ()
  "Display startup time after init."
  (message
   (format "ZyEmacs ready in %.2f seconds."
           (float-time
            (time-subtract (current-time) before-init-time)))))


(provide 'init-bench)

;;; init-bench.el ends here.
