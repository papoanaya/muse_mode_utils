;;; muse-platypus.el --- publish platypus -mom -mwww files

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
;; Muse Publishing Using platypus -mom -mwww
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)

(defgroup muse-platypus nil
  "Rules for marking up a Muse file with platypus -mom -mwww macros."
  :group 'muse-publish)

(defcustom muse-platypus-extension ".pla"
  "Default file extension for publishing platypus -mom -mwww files."
  :type 'string
  :group 'muse-platypus)

(defcustom muse-platypus-pdf-extension ".pdf"
  "Default file extension for publishing platypus -mom -mwww files to PDF."
  :type 'string
  :group 'muse-platypus)

(defcustom muse-platypus-header 
"\n\n\n\n\n\n\n\n\n\n
[ff:COURIER]
[fsize:18pt][+b]
[align:center]<lisp>(muse-publishing-directive \"title\")</lisp>[]
[-b][fsize:12pt]
[align:center]<lisp>(muse-publishing-directive \"author\")</lisp>[]
[align:center]<lisp>(muse-publishing-directive \"date\")</lisp>

[align:left]
\n\n\n
[pg]
"
  
  "Header used for publishing platypus -mom -mwww files."
  :type '(choice string file)
  :group 'muse-platypus)

(defcustom muse-platypus-footer nil
  "Footer used for publishing platypus -mom -mwww files."
  :type '(choice string file)
  :group 'muse-platypus)

(defcustom muse-platypus-markup-regexps
  `((10400 ,(concat "\\(\n</\\(blockquote\\|center\\)>\\)?\n"
                    "\\(["
                    muse-regexp-blank
                    "]*\n\\)+\\(<\\(blockquote\\|center\\)>\n\\)?")
           0 muse-platypus-markup-paragraph))
"List of markup regexps for identifying regions in a Muse page.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-platypus)

(defcustom muse-platypus-markup-functions
  '((table . muse-platypus-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-platypus)

(defcustom muse-platypus-markup-tags
  '()
  "A list of tag specifications, for specially marking up PLATYPUS."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function))
  :group 'muse-platypus)

(defcustom muse-platypus-markup-strings
  `((url             . "[+u]%2%[-u] (%1%)")
    (link            . "[+u]%2%[-u] (%1%)")
    (email-addr      . "%s")
    (emdash          . "-")
    (rule            . "")
    (no-break-space  . "")
    (line-break      . "[]")
    (enddots         . "[dots]")
    (dots            . "[dots]")
    (section         . "[+b][fsize:16pt]")
    (section-end     . "[-b][fsize:12pt]")
    (subsection      . "[+b][fsize:14pt]")
    (subsection-end  . "[-b][fsize:12pt]")
    (subsubsection   . "[+i]")
    (subsubsection-end . "[-i]")
    (begin-underline . "[+u]")
    (end-underline   . "[-u]")
    (begin-literal   . "[code]")
    (end-literal     . "[-code]")
    (begin-emph      . "[+i]")
    (end-emph        . "[-i]")
    (begin-more-emph . "[+i][+b]")
    (end-more-emph   . "[-b][-i]")
    (begin-most-emph . "[+b]")
    (end-most-emph   . "[-b]")
    (begin-verse     . "[code]\n")
    (end-verse       . "\n[-code]\n\n")
    (begin-center    . "[align:center]")
    (end-center      . "\n\n[align:left]")
    (begin-example   . "[code]\n")
    (end-example     . "\n[-code]\n\n")
    (begin-quote     . "[code]\n")
    (end-quote       . "\n[-code]\n\n")
    (begin-uli       . "[list]")
    (end-uli-item . "[]\n")
    (end-uli         . "[-list]\n\n")
    (begin-oli       . "[list|bullet:{ring}]")
    (end-oli-item . "[]\n")
    (end-oli         . "[-list]\n\n")
    (begin-dl        . "\n")
    (end-dl          .  "\n\n[paraindent:0]")
    (begin-ddt       . "[paraindent:0][+b][+u]")
    (end-ddt         . "[-u][-b]\n\n")
    (begin-dde       . "[paraindent:36pt]" )
    (end-dde         . "\n\n"))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-platypus)

(defcustom muse-platypus-markup-specials
  '((?\\ . "\\e")
    (?[ . "/[")
    (?/ . "[/]"))
    
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-platypus)

(defun muse-platypus-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (1+ (match-beginning 0)))
    (delete-region (point) end) (muse-insert-markup "\n")))

(defun muse-platypus-protect-leading-chars ()
  "Protect leading periods and apostrophes from being interpreted as
command characters."
  (while (re-search-forward "^[.']" nil t)
    (replace-match "\\\\&\\&" t)))


(defun muse-platypus-fixup-dquotes ()
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

(defun muse-platypus-prepare-buffer ()
  (goto-char (point-min))
  (muse-platypus-protect-leading-chars))

(defun muse-platypus-munge-buffer ()
  (goto-char (point-min)))

(defun muse-platypus-pdf-browse-file (file)
  (shell-command (concat "open " file)))

(defun muse-platypus-pdf-generate (file output-path final-target)
  (muse-publish-transform-output
   file output-path final-target "PDF"
   (function
    (lambda (file output-path)
      (let ((command
             (format
              (concat "file=%s; ext=%s; cd %s && cp $file$ext $file.ref && "
                      "~/platypus/platyrun -verbose $file$ext")
              (file-name-sans-extension file)
              muse-platypus-extension
              (file-name-directory output-path))))
        (shell-command command))))
   ".ps"))

;;; Register the Muse Platypus Publisher

(muse-define-style "pla"
                   :suffix    'muse-platypus-extension
                   :regexps   'muse-platypus-markup-regexps
;;;		   :functions 'muse-platypus-markup-functions
                   :strings   'muse-platypus-markup-strings
                   :tags      'muse-platypus-markup-tags
                   :specials  'muse-platypus-markup-specials
                   :before    'muse-platypus-prepare-buffer
                   :before-end 'muse-platypus-munge-buffer
                   :header    'muse-platypus-header
                   :footer    'muse-platypus-footer
                   :browser   'find-file)

(muse-derive-style "pla-pdf" "pla"
                   :final   'muse-platypus-pdf-generate
                   :browser 'muse-platypus-pdf-browse-file
                   :osuffix 'muse-platypus-pdf-extension)

(provide 'muse-platypus)

;;; muse-platypus.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
