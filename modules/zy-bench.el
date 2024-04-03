;;; zy-bench.el --- Benchmark startup. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+bench' module of the configuration.

;; This module is for benchmarking the loading time of other modules. Load this
;; module before any other modules if a benchmark is required. After startup,
;; see benchmark result via the `+bench/show' command.
;;
;; In order to benchmark everything including the loading of Zylib components,
;; this module is the only one that does not require anything.

;;; Code:

(defvar +bench-time-list '()
  "List representing the benchmark result.
Each element is a list like (FILE MSEC . INFO), where FILE is
exactly the FILE argument taken by `zy-load-rel', MSEC is the
number of milliseconds taken to load FILE, and INFO if a property
list containing additional information.

INFO can have the following keywords:

  `:label'  the label shown in the table
  `:path'   the full path of the loaded file")

;; Advise `zy-load-rel' to benchmark the loading of files.
(when (fboundp 'zy-load-rel)
  (advice-add
   #'zy-load-rel :around
   (defun +bench-time-a (oldfun file)
     "Benchmark time used to call OLDFUN on ARGS."
     (let* ((time-before (current-time))
            (result (funcall oldfun file))
            (label (if (stringp file)
                       (if (file-name-absolute-p file)
                           (file-name-nondirectory file)
                         file)
                     (symbol-name file)))
            (path (if (string-suffix-p ".elc" result)
                      (substring result 0 -1)
                    result))
            (time-elapsed (time-since time-before))
            (msec (* 1000 (float-time time-elapsed))))
       (add-to-list '+bench-time-list
                    (list file msec ':label label ':path path))
       result))))

(defun +bench-make-entry (elt msec-total)
  "Make a table entry from ELT and MSEC-TOTAL.
ELT is an element of `+bench-time-list', and MSEC-TOTAL is the
total number of milliseconds taken to load every FILE."
  (let* ((msec (cadr elt))
         (info (cddr elt))
         (percent (round (* 100 (/ msec msec-total))))
         (id (cl-gensym "+bench-entry-"))
         (file-label (plist-get info ':label))
         (file-path (plist-get info ':path))
         (file-goto-path-action (lambda (&rest _)
                                  (find-file-other-window file-path)))
         (file-props `( action ,file-goto-path-action
                        help-echo ,(format "mouse-2, RET: visit %s" file-path)))
         (file-desc (cons file-label file-props))
         (msec-str (format "%.3f" msec))
         (percent-str (format "%d%%" percent)))
    (list id (vector file-desc msec-str percent-str))))

(defun +bench-make-entries ()
  "Return entries for `+bench-result-mode'.
Entries are created from `+bench-time-list'."
  (let* ((msec-total (cl-reduce #'+ (seq-map #'cadr +bench-time-list))))
    (seq-map (lambda (pair) (+bench-make-entry pair msec-total))
             +bench-time-list)))

(defun +bench--get-entry-time (entry)
  "Get the time column of an entry ENTRY."
  (string-to-number (seq-elt (cadr entry) 1)))

(defun +bench--time-cmp-fn (e1 e2)
  "Return t if the time of E1 is shorter than E2.
E1 and E2 are tabulated list entries of `+bench-result-mode' as
returned by `+bench-make-entry'. This function is used to sort
the table based on the loading time of each file."
  (<= (+bench--get-entry-time e1)
      (+bench--get-entry-time e2)))

(define-derived-mode +bench-result-mode tabulated-list-mode "BenchResult"
  "Major mode for displaying benchmark result."
  ;; We don't want this mode to be interactively called.
  :interactive nil
  ;; Define the format.
  (setq-local tabulated-list-format
              (vector (list "File" 20 t)
                      (list "Time (ms)" 15 #'+bench--time-cmp-fn)
                      (list "Percent Time" 15 nil)))
  ;; Define the function used to make entries.
  (setq-local tabulated-list-entries #'+bench-make-entries)
  ;; Sort by time by default in a decreasing order.
  (setq-local tabulated-list-sort-key '("Time (ms)" . flip))
  ;; Create the table.
  (tabulated-list-init-header)
  ;; Print the table. Normally this is called in the listing command, but here
  ;; our can safely call it here because the entries have been determined now.
  (tabulated-list-print))

(defconst +bench-result-buffer "*BenchResult*"
  "Buffer name used to display the benchmark result.")

;; View benchmark result via `+bench/show'.
(defun +bench/show ()
  "Display benchmark result."
  (interactive)
  ;; Create the buffer and print entries.
  (with-current-buffer
      (get-buffer-create +bench-result-buffer)
    (+bench-result-mode))
  ;; Pop up the buffer.
  (pop-to-buffer +bench-result-buffer))

(provide 'zy-bench)

;;; zy-bench.el ends here
