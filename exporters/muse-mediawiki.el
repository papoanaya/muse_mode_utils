;;; muse-mediawiki.el --- publish groff -mm files

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

(defgroup muse-mediawiki nil
  "Rules for marking up a Muse file with github markup"
  :group 'muse-publish)

(defcustom muse-mediawiki-extension ".mediawiki"
  "Default file extension for publishing file with github markup"
  :type 'string
  :group 'muse-mediawiki)

(defcustom muse-mediawiki-pdf-extension nil
  "Default file extension for publishing with github markup"
  :type 'string
  :group 'muse-mediawiki)

(defcustom muse-mediawiki-header
  ""
  "Header used for publishing MD files"
  :type '(choice string file)
  :group 'muse-mediawiki)

(defcustom muse-mediawiki-footer ""
  "Footer used for publishing for MD files"
  :type '(choice string file)
  :group 'muse-mediawiki)

(defcustom muse-mediawiki-markup-regexps nil
"List of markup regexps for identifying regions in a Muse page.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-mediawiki)

(defcustom muse-mediawiki-markup-functions
  '((table . muse-mediawiki-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-mediawiki)

(defcustom muse-mediawiki-markup-tags
  '()
  "A list of tag specifications, for specially marking up GitHub."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function))
  :group 'muse-mediawiki)

(defcustom muse-mediawiki-markup-strings
  `((image-with-desc . "[File:%s|%s]")
    (image           . "[File:%s]")
    (image-link      . "[%s|%s]")

    (url             . "%s")
    (link            . "[[%s]]")
    (email-addr      . "[[mailto:%s]]")
    (emdash          . "-")
    (rule            . "----")
    (no-break-space  . "\ ")
    (line-break      . "")
    (enddots         . "....")
    (dots            . "...")
;;     (part            . "\\part{")
;;     (part-end        . "}")
     (chapter         . "=")
     (chapter-end     . "=")
    (section         . "==")
    (section-end     . "==")
    (subsection      . "===")
    (subsection-end  . "===")
    (subsubsection   . "====")
    (subsubsection-end . "====")
     (footnote        . "")
     (footnote-end    . "")
     (footnotemark    . "[[#See %d]]")
     (footnotetext    . "<div id=\"See %d\"> </div>")
;;     (footnotetext-end . "}")
    (begin-underline . "<u>")
    (end-underline   . "</u>")
    (begin-literal   . "<code>")
;;    (begin-literal-item . "+-")
    (end-literal     . "</code>")
    (begin-emph      . "'''")
    (end-emph        . "'''")
    (begin-more-emph . "'''<u>")
    (end-more-emph   . "</u>'''")
    (begin-most-emph . "'''''")
    (end-most-emph   . "'''''")
    (begin-verse     . "<blockquote>")
    (end-verse       . "</blockquote>")
    (begin-center    . "")
;;    (begin-center-item. "      ")
    (end-center      . "")
    (begin-example   . "<pre>")
    (end-example     . "</pre>")
    (begin-quote     . "<blockquote>")
;;    (begin-quote-item . "</blockquote>")
    (end-quote       . "</blockquote>")
    (begin-cite     . "''")
    (begin-cite-author . "")
    (begin-cite-year . "")
    (end-cite        . "''")
    (begin-uli       . "")
    (end-uli         . "")
    (begin-uli-item  . "* ")
    (begin-oli-item  . "# ")
    (begin-oli       . "")
    (end-oli         . "")
    (begin-dl       . "")
    (end-dl         . "")
    (begin-ddt       . ";")
    (end-ddt         . "")
    (begin-dde       . ":")
    (begin-table     . "{|")
    (end-table       . "|}")
    (begin-table-row . "|")
    (end-table-row   . "\n|-")
    (begin-table-entry . "||")
    (end-table-entry . "||")
)
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-mediawiki)

(defcustom muse-mediawiki-markup-specials
  nil
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-mediawiki)

(defun muse-mediawiki-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (1+ (match-beginning 0)))
    (delete-region (point) end)
    (muse-insert-markup "\n")
))


(defun muse-mediawiki-concat-lists ()
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

(defun muse-mediawiki-markup-table ()
  (let* ((table-info (muse-publish-table-fields (match-beginning 0)
                                                (match-end 0)))
         (row-len (car table-info))
         (field-list (cdr table-info)))
    (when table-info
;      (muse-insert-markup "{|" (make-string row-len ?l) "\n")
      (muse-insert-markup "{|\n")

      (dolist (fields field-list)
        (let ((type (car fields)))
          (setq fields (cdr fields))
          (if (eq type 'hline)
              (muse-insert-markup "|-\n")
            (when (= type 3)
              (muse-insert-markup "|-\n"))
            (muse-insert-markup "|")
            (insert (car fields))
            (setq fields (cdr fields))
            (dolist (field fields)
              (muse-insert-markup "||")
              (insert field))
            (muse-insert-markup "||\n")
            (when (= type 2)
              (muse-insert-markup "|-\n")))))
      (muse-insert-markup "|}"))))


(defun muse-mediawiki-fixup-dquotes ()
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

(defun muse-mediawiki-prepare-buffer ()
  (goto-char (point-min)))

(defun muse-mediawiki-munge-buffer ()
  (goto-char (point-min))
  (muse-mediawiki-concat-lists))

(defun muse-mediawiki-pdf-browse-file (file)
  file)

(defun muse-mediawiki-pdf-generate (file output-path final-target)
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
              muse-mediawiki-extension
              (file-name-directory output-path))))
        (shell-command command))))
   ".ps"))

;;; Register the Muse MD Publisher

(muse-define-style "mediawiki"
                   :suffix    'muse-mediawiki-extension
                   :regexps   'muse-mediawiki-markup-regexps
                   :functions 'muse-mediawiki-markup-functions
                   :strings   'muse-mediawiki-markup-strings
                   :tags      'muse-mediawiki-markup-tags
                   :specials  'muse-mediawiki-markup-specials
                   :before    'muse-mediawiki-prepare-buffer
                   :before-end 'muse-mediawiki-munge-buffer
                   :header    'muse-mediawiki-header
                   :footer    'muse-mediawiki-footer
                   :browser   'find-file)

;;(muse-derive-style "md-pdf" "md"
;;                   :final   'muse-mediawiki-pdf-generate
;;                   :browser 'muse-mediawiki-pdf-browse-file
;;                   :osuffix 'muse-mediawiki-pdf-extension)

(provide 'muse-mediawiki)

;;; muse-mediawiki.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
