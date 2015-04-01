;;; muse-plus.el --- Google Plus Markup

;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2012
;;   Free Software Foundation, Inc.

;; Author: Luis Roberto Anaya
;; Date: Sun 24-Jul-2012

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
;; Muse Publishing Using Github Markup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)

(defgroup muse-plus nil
  "Rules for marking up a Muse file with github markup"
  :group 'muse-publish)

(defcustom muse-plus-extension ".plus"
  "Default file extension for publishing file with github markup"
  :type 'string
  :group 'muse-plus)

(defcustom muse-plus-pdf-extension nil
  "Default file extension for publishing with github markup"
  :type 'string
  :group 'muse-plus)

(defcustom muse-plus-header
  ""
  "Header used for publishing Plus files"
  :type '(choice string file)
  :group 'muse-plus)

(defcustom muse-plus-footer ""
  "Footer used for publishing for Plus files"
  :type '(choice string file)
  :group 'muse-plus)

(defcustom muse-plus-markup-regexps nil
"List of markup regexps for identifying regions in a Muse page.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-plus)

(defcustom muse-plus-markup-functions
  '((table . muse-plus-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-plus)

(defcustom muse-plus-markup-tags
  '()
  "A list of tag specifications, for specially marking up GitHub."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function))
  :group 'muse-plus)

(defcustom muse-plus-markup-strings
  `((image-with-desc . "%2% (%1%)")
    (image           . "%2% (%1%)")
    (image-link      . "%2% (%1%)")
    (url             . "%2% (%1%)")
    (link            . "%2% (%1%)")
    (email-addr      . "%2% (%1%)")
    (emdash          . "-")
    (rule            . "")
    (no-break-space  . "\ ")
    (line-break      . "\n")
    (enddots         . "....")
    (dots            . "...")
;;     (part            . "\\part{")
;;     (part-end        . "}")
;;     (chapter         . "\\chapter{")
;;     (chapter-end     . "}")
    (section         . "*")
    (section-end     . "*")
    (subsection      . "*_")
    (subsection-end  . "_*")
    (subsubsection   . "_")
    (subsubsection-end . "_")
;;     (footnote        . "")
;;     (footnote-end    . "")
;;     (footnotemark    . "\\footnotemark[%d]")
;;     (footnotetext    . "\\footnotetext[%d]{")
;;     (footnotetext-end . "}")
    (begin-underline . "_")
    (end-underline   . "_")
    (begin-literal   . "-")
;;    (begin-literal-item . "")
    (end-literal     . "-")
    (begin-emph      . "_")
    (end-emph        . "_")
    (begin-more-emph . "*")
    (end-more-emph   . "*")
    (begin-most-emph . "*_")
    (end-most-emph   . "_*")
    (begin-verse     . "")
    (end-verse       . "")
    (begin-center    . "")
    (begin-center-item. "")
    (end-center      . "")
    (begin-example   . "")
    (end-example     . "")
;    (begin-quote     . "")
    (begin-quote-item . "_")
    (end-quote-item . "_")
;    (end-quote       . "</blockquote>\n")
;    (begin-cite     . "")
;    (begin-cite-author . "")
;    (begin-cite-year . "")
;    (end-cite        . "")
    (begin-uli       . "")
    (end-uli         . "")
    (begin-uli-item  . "* ")
    (begin-oli-item  . "* ")
    (begin-oli       . "")
    (end-oli         . "")
    (begin-dl       . "")
    (end-dl         . "")
    (begin-ddt       . "*")
    (end-ddt         . "* :")
    (begin-dde       . "     ")
    (end-dde       . ""))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-plus)

(defcustom muse-plus-markup-specials
  nil
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-plus)

(defun muse-plus-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (1+ (match-beginning 0)))
    (delete-region (point) end)
    (muse-insert-markup "\n")
))


(defun muse-plus-concat-lists ()
  "Join like lists."
  (let ((type "")
        arg begin)
    (while (re-search-forward "^\-[ \t]+\\(.*\\)\n" nil t)
      (setq arg (match-string 1))
      (if (string= arg "OFF")
          (setq begin (match-beginning 0))
        (if (and begin (string= type arg))
            (delete-region begin (match-end 0))
          (setq type arg
                begin 0))))))

(defun muse-plus-fixup-dquotes ()
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

(defun muse-plus-prepare-buffer ()
  (goto-char (point-min)))

(defun muse-plus-munge-buffer ()
  (goto-char (point-min))
  (muse-plus-concat-lists))

(defun muse-plus-pdf-browse-file (file)
  file)

(defun muse-plus-pdf-generate (file output-path final-target)
  (muse-publish-transform-output
   file output-path final-target "PDF"
   (function
    (lambda (file output-path)
      (let ((command
             (format
              (concat "file=%s; ext=%s; cd %s && cp $file$ext $file.ref && "
                      "groff -mm -t $file$ext > $file.ps && "
                      "ps2pdf $file.ps")
              (file-name-sans-extension file)
              muse-plus-extension
              (file-name-directory output-path))))
        (shell-command command))))
   ".ps"))

;;; Register the Muse MD Publisher

(muse-define-style "plus"
                   :suffix    'muse-plus-extension
                   :regexps   'muse-plus-markup-regexps
;;;		   :functions 'muse-plus-markup-functions
                   :strings   'muse-plus-markup-strings
                   :tags      'muse-plus-markup-tags
                   :specials  'muse-plus-markup-specials
                   :before    'muse-plus-prepare-buffer
                   :before-end 'muse-plus-munge-buffer
                   :header    'muse-plus-header
                   :footer    'muse-plus-footer
                   :browser   'find-file)

;(muse-derive-style "md-pdf" "md" 
;                    :final   'muse-plus-pdf-generate
;                    :browser 'muse-plus-pdf-browse-file
;                    :osuffix 'muse-plus-pdf-extension) 

(provide 'muse-plus)

;;; muse-plus.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
