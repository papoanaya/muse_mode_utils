;;; muse-bbcode.el --- publish bbcode -mom -mwww files

;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
;;   Free Software Foundation, Inc.

;; Author: Andrew J. Korty (ajk AT iu DOT edu)
;; Date: Tue 5-Jul-2005

;; This file is part of Emacs Muse.  It is not part of GNU Emacs.

;; Emacs Muse is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; Emacs Muse is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs Muse; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Contributors:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Publishing Using bbcode -mom -mwww
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)

(defgroup muse-bbcode nil
  "Rules for marking up a Muse file with bbcode -mom -mwww macros."
  :group 'muse-publish)

(defcustom muse-bbcode-extension ".bbc"
  "Default file extension for publishing bbcode -mom -mwww files."
  :type 'string
  :group 'muse-bbcode)

(defcustom muse-bbcode-pdf-extension ".pdf"
  "Default file extension for publishing bbcode -mom -mwww files to PDF."
  :type 'string
  :group 'muse-bbcode)

(defcustom muse-bbcode-header nil
  "Header used for publishing bbcode -mom -mwww files."
  :type '(choice string file)
  :group 'muse-bbcode)

(defcustom muse-bbcode-footer nil
  "Footer used for publishing bbcode -mom -mwww files."
  :type '(choice string file)
  :group 'muse-bbcode)

(defcustom muse-bbcode-markup-regexps
  `((10400 ,(concat "\\(\n</\\(blockquote\\|center\\)>\\)?\n"
                    "\\(["
                    muse-regexp-blank
                    "]*\n\\)+\\(<\\(blockquote\\|center\\)>\n\\)?")
           0 muse-bbcode-markup-paragraph))
"List of markup regexps for identifying regions in a Muse page.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-bbcode)

(defcustom muse-bbcode-markup-functions
  '((table . muse-bbcode-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-bbcode)

(defcustom muse-bbcode-markup-tags
  '()
  "A list of tag specifications, for specially marking up BBCODE."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function))
  :group 'muse-bbcode)

(defcustom muse-bbcode-markup-strings
  `((url             . "[url=%1%]%2%[/url]")
    (link            . "[url=%1%]%2%[/url]")
    (image-with-desc . "[table]\n[tr][td][img] %1%.%2%[/img][/td][/tr]\n[tr][td]%3%[/td][/tr]\n[/table]\n")
    (image           . "[img]%s.%s[/img]")
    (image-link      . "[url]%s[/url]\n[img]%s.%s[/img]\n")
    (email-addr      . "%s")
    (emdash          . "-")
    (rule            . "")
    (no-break-space  . "")
    (line-break      . "[]")
    (enddots         . "...")
    (dots            . "...")
    (section         . "[b][size=16]")
    (section-end     . "[/size][/b]")
    (subsection      . "[b][size=14]")
    (subsection-end  . "[/size][/b]")
    (subsubsection   . "[i]")
    (subsubsection-end . "[/i]")
    (begin-underline . "[u]")
    (end-underline   . "[/u]")
    (begin-literal   . "[code]")
    (end-literal     . "[/code]")
    (begin-emph      . "[i]")
    (end-emph        . "[/i]")
    (begin-more-emph . "[i][b]")
    (end-more-emph   . "[/b][/i]")
    (begin-most-emph . "[b]")
    (end-most-emph   . "[/b]")
    (begin-verse     . "[code]\n")
    (end-verse       . "\n[/code]\n")
    (begin-center    . "[center]")
    (end-center      . "[/center]")
    (begin-example   . "[code]\n")
    (end-example     . "\n[/code]\n\n")
    (begin-quote     . "[quote]\n")
    (end-quote       . "\n[/quote]\n")
    (begin-uli       . "[list]\n")
    (begin-uli-item . "[*]")
    (end-uli         . "\n[/list]\n")
    (begin-oli       . "[list=1]\n")
    (begin-oli-item . "[*]")
    (end-oli         . "\n[/list]")
    (begin-dl        . "[table border=0]")
    (end-dl          .  "\n[/table]")
    (begin-ddt       . "[tr][td][b]")
    (end-ddt         . "[/b][/td]")
    (begin-dde       . "[td]" )
    (end-dde         . "[/td][/tr]"))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-bbcode)

(defcustom muse-bbcode-markup-specials nil
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-bbcode )

(defun muse-bbcode-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (1+ (match-beginning 0)))
    (delete-region (point) end) (muse-insert-markup "\n")))

(defun muse-bbcode-protect-leading-chars ()
  "Protect leading periods and apostrophes from being interpreted as
command characters."
  (while (re-search-forward "^[.']" nil t)
    (replace-match "\\\\&\\&" t)))


(defun muse-bbcode-fixup-dquotes ()
  "Fixup double quotes."
  (let ((open t))
    (while (search-forward "\"" nil t)
      (unless (get-text-property (match-beginning 0) 'read-only)
        (if (and (bolp) (eq (char-before) ?\n))
            (setq open t))
        (if open
            (progn
              (replace-match "``")
              (setq open nil))
          (replace-match "''")
          (setq open t))))))

(defun muse-bbcode-prepare-buffer ()
  (goto-char (point-min))
  (muse-bbcode-protect-leading-chars))

(defun muse-bbcode-munge-buffer ()
  (goto-char (point-min)))

(defun muse-bbcode-pdf-browse-file (file)
  (shell-command (concat "open " file)))

(defun muse-bbcode-pdf-generate (file output-path final-target)
  (muse-publish-transform-output
   file output-path final-target "PDF"
   (function
    (lambda (file output-path)
      (let ((command
             (format
              (concat "file=%s; ext=%s; cd %s && cp $file$ext $file.ref && "
                      "~/bbcode/platyrun -verbose $file$ext")
              (file-name-sans-extension file)
              muse-bbcode-extension
              (file-name-directory output-path))))
        (shell-command command))))
   ".ps"))

;;; Register the Muse Bbcode Publisher

(muse-define-style "bbc"
                   :suffix    'muse-bbcode-extension
                   :regexps   'muse-bbcode-markup-regexps
;;;		   :functions 'muse-bbcode-markup-functions
                   :strings   'muse-bbcode-markup-strings
                   :tags      'muse-bbcode-markup-tags
                   :specials  'muse-bbcode-markup-specials
                   :before    'muse-bbcode-prepare-buffer
                   :before-end 'muse-bbcode-munge-buffer
                   :header    'muse-bbcode-header
                   :footer    'muse-bbcode-footer
                   :browser   'find-file)

(muse-derive-style "bbc-pdf" "bbc"
                   :final   'muse-bbcode-pdf-generate
                   :browser 'muse-bbcode-pdf-browse-file
                   :osuffix 'muse-bbcode-pdf-extension)

(provide 'muse-bbcode)

;;; muse-bbcode.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
