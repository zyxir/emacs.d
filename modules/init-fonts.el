;;; init-fonts.el --- Font settings.  -*- lexical-binding: t -*-
;;; Commentary:

;; This file sets font for various faces and multiple character sets. An
;; objective of these settings is to make the width of one Chinese character
;; equal that of two Latin characters, so that these two lines could align:
;;
;; 這句話一共由十三個漢字組成
;; abcdefghijklmnopqrstuvwxyz

;;; Code:

(eval-and-compile (require 'init-basic))

(pkg! 'ligature)

;;;; Font Setting Utility

(defconst zy/cjk-charsets '(han cjk-misc bopomofo kana hangul)
  "CJK character sets.")

(defun zy/set-face-charset-font (face frame charset font)
  "Set the font used for character set CHARSET in face FACE.

This function has no effect if `display-graphic-p' returns nil,
since fontset is not supported in console mode.

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
               (format "-*-*-*-*-*--*-*-*-*-*-*-fontset-zy%s"
                       (cl-gensym))
               nil)))
      ;; Set font for the fontset
      (if (listp charset)
          (mapc (lambda (c)
                  (set-fontset-font fontset c font frame 'prepend))
                charset)
        (set-fontset-font fontset charset font frame))
      ;; Assign the fontset to the face if necessary
      (when unspecified-p
        (set-face-attribute face frame :fontset fontset)))))

;;;; Font Customizables

(defun zy/-set-font (font-sym font-val)
  "The `:set' function for customizable font variables.
This function set the value of FONT-SYM to FONT-VAL, and run
`zy/setup-font-faces' if it is ready."
  (set font-sym font-val)
  (when (fboundp 'zy/setup-font-faces)
    (zy/setup-font-faces)))

(defcustom zy/font-size 16
  "The pixel size of font in `default' face."
  :type 'integer
  :group 'zyemacs
  :set #'zy/-set-font)

(defmacro zy/define-font (name font docstring)
  "Define a customizable font variable NAME.
FONT is its default value, and DOCSTRING is the documentation
string. The defined customizable variable will have the
`zy/setup-font-faces' as its `:set' function."
  (declare (doc-string 3) (indent defun))
  `(defcustom ,name ,font ,docstring
     :type 'string
     :group 'emacs
     :set 'zy/-set-font))

(zy/define-font zy/font-default "Sarasa Mono HC"
  "Font for the `default' face.")
(zy/define-font zy/font-default-cjk "Sarasa Mono HC"
  "CJK font for the `default' face.")
(zy/define-font zy/font-varpitch "Noto Sans"
  "Font for the `variable-pitch' face.")
(zy/define-font zy/font-varpitch-cjk "Noto Sans CJK TC"
  "CJK font for the `variable-pitch' face.")

;;;; The Actual Setup

;; I used to write very flexible font configuration codes that defines a tons of
;; faces and automatically picks the first available font from a list, but that
;; turned out to be too complicated and heavy. Now I just hard-coded the font
;; names and rely on the default font fallback mechanism.

;; Anyway this is just my personal configuration, I can change the code at any
;; time.

(defun zy/-setup-font-faces-now (frame)
  "Setup font faces according to font variables.

This is used in `after-make-frame-functions', and arugment
FRAME makes sure that the validness of the fonts can be correctly
checked.

This should be only used when FRAME is an X frame. For
interactive use, use `zy/setup-font-faces' instead."
  (let ((available-fonts (font-family-list frame))
        (height (round (* zy/font-size 7.5))))
    ;; Default face, fixed-pitch face and global font size.
    (if (member zy/font-default available-fonts)
        (progn
          (set-face-attribute 'default nil
                              :family zy/font-default
                              :height height)
          (set-face-attribute 'fixed-pitch nil
                              :family zy/font-default))
      (set-face-attribute 'default nil
                          :height height))
    (when (member zy/font-default-cjk available-fonts)
      (zy/set-face-charset-font 'default nil
                                zy/cjk-charsets zy/font-default-cjk)
      (zy/set-face-charset-font 'fixed-pitch nil
                                zy/cjk-charsets zy/font-default-cjk))
    ;; Variable-pitch face.
    (when (member zy/font-varpitch available-fonts)
      (set-face-attribute 'variable-pitch nil
                          :family zy/font-varpitch))
    (when (member zy/font-varpitch-cjk available-fonts)
      (zy/set-face-charset-font 'variable-pitch nil
                                zy/cjk-charsets zy/font-varpitch-cjk))))

(after-gui! (zy/-setup-font-faces-now frame))

;;;; Ligatures

(after-gui!
  (require 'ligature)
  (ligature-set-ligatures
   'prog-mode
   ;; These ligatures are for Iosevka and also apply to Sarasa Gothic.
   '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
     "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
     "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
     "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
     "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
     "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
     ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
     "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
     "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
     "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
     "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  (global-ligature-mode 1))

(provide 'init-fonts)

;;; init-fonts.el ends here
