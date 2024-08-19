;;; zy-font.el --- Setup fonts. -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides the `+font' module of the configuration.

;; It sets font for various faces and multiple character sets. Currently the
;; default font is JetBrains Mono NL Nerd Font, the variable pitch font is Noto
;; Sans, and the Chinese font for both faces is Sarasa Fixed SC (which comes
;; from Source Han Sans SC).
;;
;; Additionally, it implements a face named `+font-serif', which uses serif font
;; for text display. The `+font/serif-mode' is like `variable-pitch-mode' but
;; remaps the default face to `+font-serif' instead of `variable-pitch'.
;;
;; Tweak fonts until all following text displays:
;;
;; [Chinese] 業精於勤，荒於嬉；行成於思，毀於隨。
;; [English] Sphinx of black quartz, judge my vow.
;; [Russian] Эх, чужак! Общий съём цен шляп (юфть)—вдрызг!

;;; Code:

(require 'zylib)

;;;; Font Customizables

(defvar +font-no-refresh-on-set nil
  "Do no refresh font when customizing them.
This applies to all customizatble variables in the customization
group `+font'.")

(defun +font--set-var (font-sym font-val)
  "The `:set' function for customizable font variables.
This function set the value of FONT-SYM to FONT-VAL, and run
`+font/setup' if it is ready."
  (set font-sym font-val)
  (when (and (not +font-no-refresh-on-set)
             (fboundp '+font/setup))
    (+font/setup)))

(defcustom +font-size 16
  "The pixel size of font for the `default' face."
  :type 'integer
  :group '+font
  :set #'+font--set-var)

(defcustom +font-varpitch-relsize 0
  "The relative pixel size of font for the `variable-pitch' face.
If it is an integer, it is the relative pixel size. Otherwise it
should be a float, which stands for the proportional size."
  :type '(choice integer float)
  :group '+font
  :set #'+font--set-var)

(defcustom +font-serif-relsize 1.375
  "The relative pixel size of font for the `+font-serif' face.
If it is an integer, it is the relative pixel size. Otherwise it
should be a float, which stands for the proportional size."
  :type '(choice integer float)
  :group '+font
  :set #'+font--set-var)

(defface +font-serif nil
  "Face with serif fonts.")

(autoload 'buffer-face-mode-invoke "face-remap")

(defun +font/serif-mode (&optional arg)
  "Use `+font-serif' as default with ARG.
An interface to `buffer-face-mode' which uses the `+font-serif'
face. Besides the choice of face, it is the same as
`buffer-face-mode'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (buffer-face-mode-invoke '+font-serif (or arg t)
                           (called-interactively-p 'interactive)))

(defmacro +font-define-font (name font docstring)
  "Define a customizable font variable NAME.
FONT is its default value, and DOCSTRING is the documentation
string. The defined customizable variable will have the
`zy/setup-font-faces' as its `:set' function."
  (declare (doc-string 3) (indent defun))
  `(defcustom ,name ,font ,docstring
     :type 'string
     :group '+font
     :set '+font--set-var))

;; Set `+font-no-refresh-on-set' temporarily to define font or read customized
;; font without triggering automatic font refreshes. A font refresh will be
;; performed manually afterwards. We cannot use `let' here, since `let' creates
;; a lexical scope, preventing the defined variables from being used globally.
(setq +font-no-refresh-on-set t)

(+font-define-font +font-default "JetBrainsMonoNL Nerd Font Mono"
  "Font for the `default' face.")
(+font-define-font +font-default-cjk "Sarasa Fixed SC Nerd Font"
  "CJK font for the `default' face.")
(+font-define-font +font-varpitch "Noto Sans"
  "Font for the `variable-pitch' face.")
(+font-define-font +font-varpitch-cjk "Sarasa Fixed SC Nerd Font"
  "CJK font for the `variable-pitch' face.")
(+font-define-font +font-serif "Noto Serif"
  "Font for the `+font-serif' face.")
(+font-define-font +font-serif-cjk "Source Han Serif HC"
  "CJK font for the `+font-serif' face.")

;; Unset `+font-no-refresh-on-set', so that if the user customizes a font, a
;; font refresh will be triggered.
(setq +font-no-refresh-on-set nil)

;;;; Font Setting Utility

(defconst +font-cjk-charsets '(han cjk-misc bopomofo kana hangul)
  "CJK character sets.")

(defun +font-set-charset-font (face frame charset font)
  "Set the font used for character set CHARSET in face FACE.

This function has no effect if `display-graphic-p' returns nil
for FRAME, since fontset is not supported in console mode.

FRAME specifies the frame to set in. When FRAME is nil or
omitted, set it for all existing frames, as well as the default
for new frames.

CHARSET specifies the character set to set font for. CHARSET
could also be a list of character sets, where every character set
will be set for.

FONT is the font to be set. It can be a `font-spec' object, or a
font name string.

This is a convenient method to set font for specific character
set (like CJK characters or symbols). However, the fontset system
of Emacs is complicated, and not very straightforward. Instead of
playing with `font-spec', fontsets and frame attributes, this
function provides a simpler interface that just work."
  (when (display-graphic-p frame)
    (let* (;; The fontset that we are going to manipulate
           (fontset (face-attribute face :fontset frame))
           ;; If the fontset is not specified
           (unspecified-p (equal fontset 'unspecified)))
      ;; If the fontset is not specified, create a new one with a
      ;; programmatically generated name
      (when unspecified-p
        (setq fontset
              (new-fontset
               (format "-*-*-*-*-*--*-*-*-*-*-*-fontset-zy%s"
                       (cl-gensym))
               nil)))
      ;; Set font for the fontset
      (if (listp charset)
          (mapc (lambda (c)
                  (set-fontset-font fontset c font nil 'prepend))
                charset)
        (set-fontset-font fontset charset font nil))
      ;; Assign the fontset to the face if necessary
      (when unspecified-p
        (set-face-attribute face frame :fontset fontset)))))

(defun +font-relsize-to-height (relsize)
  "Convert RELSIZE to height.
RELSIZE is an integer or float like `+font-varpitch-relsize'. It
is converted to a relative `:height' attribute as described in
`set-face-attribute'."
  (cond
   ((integerp relsize)
    #'(lambda (h) (+ h (round (* relsize 7.5)))))
   ((floatp relsize) relsize)
   (t (error "Invalid relative font size: %s" relsize))))

;;;; The Actual Setup

;; I used to write very flexible font configuration codes that defines a tons of
;; faces and automatically picks the first available font from a list, but that
;; turned out to be too complicated and heavy. Now I just hard-coded the font
;; names and rely on the default font fallback mechanism.

;; Anyway this is just my personal configuration, I can change the code at any
;; time.

(defun +font--setup-now (frame)
  "Setup font faces according to font variables.

This is used in `after-make-frame-functions', and arugment
FRAME makes sure that the validness of the fonts can be correctly
checked."
  (let ((available-fonts (font-family-list frame))
        (height (round (* +font-size 7.5))))
    ;; `default'/`fixed-pitch' face and global font size.
    (when (member +font-default available-fonts)
      (set-face-attribute 'default nil :family +font-default)
      (set-face-attribute 'fixed-pitch nil :family +font-default))
    (when height
      (set-face-attribute 'default nil :height height))
    (set-face-attribute 'fixed-pitch nil :height 1.0)
    (when (member +font-default-cjk available-fonts)
      (+font-set-charset-font
       'default nil +font-cjk-charsets +font-default-cjk)
      (+font-set-charset-font
       'fixed-pitch nil +font-cjk-charsets +font-default-cjk))
    ;; `variable-pitch' face.
    (when (member +font-varpitch available-fonts)
      (set-face-attribute 'variable-pitch nil :family +font-varpitch))
    (when +font-varpitch-relsize
      (set-face-attribute 'variable-pitch nil :height
                          (+font-relsize-to-height +font-varpitch-relsize)))
    (when (member +font-varpitch-cjk available-fonts)
      (+font-set-charset-font
       'variable-pitch nil +font-cjk-charsets +font-varpitch-cjk))
    ;; The custom `+font-serif' face.
    (when (member +font-serif available-fonts)
      (set-face-attribute '+font-serif nil :family +font-serif))
    (when (member +font-serif-cjk available-fonts)
      (+font-set-charset-font
       '+font-serif nil +font-cjk-charsets +font-serif-cjk))
    (when +font-serif-relsize
      (set-face-attribute '+font-serif nil :height
                          (+font-relsize-to-height +font-serif-relsize)))))

(defun +font/setup (&optional frame)
  "Setup font faces according to font variables.
This sets fonts for FRAME and all future graphic frames. If FRAME
is not graphic, which possibly means that graphics is not ready
yet, defer the setting until the first graphic frame is created.

This function can also be interactively called to fix font
issues."
  (interactive)
  (setq frame (if frame frame (selected-frame)))
  (if (display-graphic-p frame)
      (progn
        (remove-hook 'after-make-frame-functions '+font/setup)
        (+font--setup-now frame))
    (add-hook 'after-make-frame-functions '+font/setup)))

(+font/setup)

(provide 'zy-font)

;;; zy-font.el ends here
