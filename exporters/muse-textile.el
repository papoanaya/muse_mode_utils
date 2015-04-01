;;; muse-textile.el --- Google Textile Markup

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

(defgroup muse-textile nil
  "Rules for marking up a Muse file with github markup"
  :group 'muse-publish)

(defcustom muse-textile-extension ".textile"
  "Default file extension for publishing file with github markup"
  :type 'string
  :group 'muse-textile)

(defcustom muse-textile-pdf-extension nil
  "Default file extension for publishing with github markup"
  :type 'string
  :group 'muse-textile)

(defcustom muse-textile-header
  ""
  "Header used for publishing Textile files"
  :type '(choice string file)
  :group 'muse-textile)

(defcustom muse-textile-footer ""
  "Footer used for publishing for Textile files"
  :type '(choice string file)
  :group 'muse-textile)

(defcustom muse-textile-markup-regexps nil
"List of markup regexps for identifying regions in a Muse page.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-textile)

(defcustom muse-textile-markup-functions
  '((table . muse-textile-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-textile)

(defcustom muse-textile-markup-tags
  '()
  "A list of tag specifications, for specially marking up GitHub."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function))
  :group 'muse-textile)

(defcustom muse-textile-markup-strings
  `((image-with-desc . "%1%.%2% (%3%)")
    (image           . "!%s.%s!")
    (image-link      . "!%s.%s!:%s")
    (url             . "\"%2%\":%1%")
    (link            . "\"%2%\":%1%")
    (email-addr      . "\"%2%\":%1%")
    (emdash          . "--")
    (rule            . "-----")
    (no-break-space  . "\ ")
    (line-break      . "\n")
;;    (line-break . "")
    (enddots         . "....")
    (dots            . "...")
;;     (part            . "\\part{")
;;     (part-end        . "}")
;;     (chapter         . "\\chapter{")
;;     (chapter-end     . "}")
    (section         . "h1. ")
    (section-end     . "\n")
    (subsection      . "h2. ")
    (subsection-end  . "\n")
    (subsubsection   . "h3. ")
    (subsubsection-end . "\n")
;;     (footnote        . "")
;;     (footnote-end    . "")
     (footnotemark    . "[%d]")
     (footnotetext    . "fn%d. ")
;;     (footnotetext-end . "")
    (begin-underline . "_")
    (end-underline   . "_")
    (begin-literal   . "@")
    (begin-literal-item . "")
    (end-literal-item . "")
    (end-literal     . "@")
    (begin-emph      . "*")
    (end-emph        . "*")
    (begin-more-emph . "**")
    (end-more-emph   . "**")
    (begin-most-emph . "**")
    (end-most-emph   . "**")
    (begin-verse     . "??")
    (end-verse       . "??")
    (begin-center    . "")
    (begin-center-item. "=")
    (end-center      . "")
    (begin-example   . "pre. ")
    (end-example     . "")
    (begin-quote     . "bq. ")
    (begin-quote-item . "")
    (end-quote-item . "")
    (end-quote       . "\n")
    (begin-cite     . "??")
;    (begin-cite-author . "")
;    (begin-cite-year . "")
    (end-cite        . "??")
    (begin-uli       . "")
    (end-uli         . "")
    (begin-uli-item  . "* ")
    (begin-oli-item  . "# ")
    (end-uli-item  . "")
    (end-oli-item  . "")
    (begin-oli       . "")
    (end-oli         . "")
    (begin-dl       . "")
    (end-dl         . "")
    (begin-ddt       . "**")
    (end-ddt         . "**")
    (begin-dde       . ": ")
    (end-dde       . "")
    (begin-table     . "|")
    (end-table       . "|")
    (begin-table-row . "|")
    (end-table-row   . "\n")
    (begin-table-entry . "|")
    (end-table-entry . "|")

)
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-textile)

(defcustom muse-textile-markup-specials
  nil
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-textile)

(defun muse-textile-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (1+ (match-beginning 0)))
    (delete-region (point) end)
    (muse-insert-markup "\n")
))


(defun muse-textile-concat-lists ()
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

(defun muse-textile-fixup-dquotes ()
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

(defun muse-textile-prepare-buffer ()
  (goto-char (point-min)))

(defun muse-textile-munge-buffer ()
  (goto-char (point-min))
  (muse-textile-concat-lists))

(defun muse-textile-pdf-browse-file (file)
  file)

(defun muse-textile-pdf-generate (file output-path final-target)
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
              muse-textile-extension
              (file-name-directory output-path))))
        (shell-command command))))
   ".ps"))


(defun muse-textile-markup-table ()
  (let* ((table-info (muse-publish-table-fields (match-beginning 0)
                                                (match-end 0)))
         (row-len (car table-info))
         (field-list (cdr table-info)))
    (when table-info
;      (muse-insert-markup "{|" (make-string row-len ?l) "\n")
;      (muse-insert-markup "|\n")

      (dolist (fields field-list)
        (let ((type (car fields)))
          (setq fields (cdr fields))
          (if (eq type 'hline)
              (muse-insert-markup "|\n")
            (when (= type 3)
              (muse-insert-markup "|\n"))
            (muse-insert-markup "|")
            (insert (car fields))
            (setq fields (cdr fields))
            (dolist (field fields)
              (muse-insert-markup "|")
              (insert field))
            (muse-insert-markup "|\n")
            (when (= type 2)
              (muse-insert-markup "|\n")))))
;      (muse-insert-markup "|")
)))


;;; Register the Muse MD Publisher

(muse-define-style "textile"
                   :suffix    'muse-textile-extension
                   :regexps   'muse-textile-markup-regexps
                   :functions 'muse-textile-markup-functions
                   :strings   'muse-textile-markup-strings
                   :tags      'muse-textile-markup-tags
                   :specials  'muse-textile-markup-specials
                   :before    'muse-textile-prepare-buffer
                   :before-end 'muse-textile-munge-buffer
                   :header    'muse-textile-header
                   :footer    'muse-textile-footer
                   :browser   'find-file)

;(muse-derive-style "md-pdf" "md" 
;                    :final   'muse-textile-pdf-generate
;                    :browser 'muse-textile-pdf-browse-file
;                    :osuffix 'muse-textile-pdf-extension) 

(provide 'muse-textile)

;;; muse-textile.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
