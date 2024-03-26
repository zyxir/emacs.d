;;; zy-journal.el --- Journal-keeping. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+journal' module of the configuration.

;; I keep journal in Org via Org-journal. It is simple and effective. To make to
;; most of Org-journal, the built-in Calendar package is also configured here.

;;; Code:

(require 'zylib)

(pkg! 'org-journal)

(after! '+leader
  (keybind! nil +leader-o-map
    "j" (cons "Journal" #'org-journal-new-entry)))

;; Require Org-journal when running as daemon.
(daemon-require! 'org-journal)

(after! 'org-journal
  ;; The following settings are applied for all my old journals, and work well
  ;; for me. Changing them may render my old journals undetectable.
  (setq org-journal-file-format "%F.org"
        org-journal-date-format "%F %a W%V\n"
        org-journal-date-prefix "#+title: "
        org-journal-time-format "%R "
        org-journal-time-format-post-midnight "%R (midnight) "
        org-journal-time-prefix "\n* "
        org-journal-file-header "")

  ;; Setup keybindings inside a journal buffer.
  (defprefix! +journal-map "Journal"
              nil org-journal-mode-map "<localleader>"
    "h" (cons "Prev" #'org-journal-previous-entry)
    "j" (cons "Next" #'org-journal-next-entry)
    "k" (cons "Prev" #'org-journal-previous-entry)
    "l" (cons "Next" #'org-journal-next-entry)
    "/" (cons "Search" #'org-journal-search)))

;; Require Org-journal when Calendar is loaded, so that Org-journal properly
;; embeds itself into the calendar.
(after! 'calendar (require 'org-journal))

(after! '(calendar org-journal)
  ;; HACK: When moving to previous/next journal entry in the calendar,
  ;; Org-journal always re-computes the list of entries, which is fairly slow
  ;; when filesystem operations are expensive (for instance, when accessing
  ;; files on the Windows host from WSL). We rewrite a system to move between
  ;; journal dates without re-computation. However, when an entry is
  ;; added/removed, manually refreshing the calendar buffer is required to move
  ;; around correctly.
  (eval-and-compile
    (defun +journal-dates ()
      "Get the list of journal dates.
This uses the hash table `org-journal--dates', which is updated
when manually refresh the calendar buffer."
      (let ((dates '()))
        (maphash (lambda (k _v) (push k dates)) org-journal--dates)
        dates))

    (defun +journal--next-in-calendar (&optional prev)
      "Go to the next journal netry in the calendar.
If PREV is non-nil, go to the previous entry instead."
      (let* ((dates (+journal-dates))
             (dates (if prev (reverse dates) dates))
             (earlier-fn #'org-journal--calendar-date-compare)
             (cmp-fn (if prev earlier-fn
                       (lambda (date1 date2) (funcall earlier-fn date2 date1))))
             (cursor-date (calendar-cursor-to-date))
             (pred (lambda (dates)
                     (and dates
                          (funcall cmp-fn (car dates) cursor-date)
                          (if (cdr-safe dates)
                              (not (funcall cmp-fn (cadr dates) cursor-date))
                            t)))))
        (while (and dates (not (funcall pred dates)))
          (setq dates (cdr dates)))
        (if dates
            (progn (calendar-goto-date (car dates))
                   (when org-journal-follow-mode
                     (org-journal-display-entry nil)))
          (user-error "No %s entry" (if prev "previous" "next")))))

    (defun +journal-next-in-calendar ()
      "Go to the next journal entry in the calendar."
      (interactive)
      (+journal--next-in-calendar))

    (defun +journal-prev-in-calendar ()
      "Go to the previous journal entry in the calendar."
      (interactive)
      (+journal--next-in-calendar 'prev)))

  (keybind! 'motion calendar-mode-map
    "[ j" (cons "Prev Journal" #'+journal-prev-in-calendar)
    "] j" (cons "Next Journal" #'+journal-next-in-calendar))

  (defprefix! +journal-calendar-map "Journal"
              nil calendar-mode-map "<localleader>"
    "h" (cons "Prev" #'+journal-prev-in-calendar)
    "j" (cons "Next" #'+journal-next-in-calendar)
    "k" (cons "Prev" #'+journal-prev-in-calendar)
    "l" (cons "Next" #'+journal-next-in-calendar)
    "/" (cons "Search" #'org-journal-search)
    "m" (cons "Mark" #'org-journal-mark-entries)
    "r" (cons "Read" #'org-journal-read-entry)
    "d" (cons "Read in O.W." #'org-journal-display-entry)
    "n" (cons "New" #'org-journal-new-date-entry)))

(provide 'zy-journal)

;;; zy-journal.el ends here
