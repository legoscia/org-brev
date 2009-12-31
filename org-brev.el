;;; org-brev.el --- export Org files to LaTeX using the brev class

;; Copyright (C) 2009  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module enables exporting Org files to the LaTeX `brev' class.
;; The `brev' class is a class for writing letters, that can be found
;; at <http://www.asgeirnilsen.com/latex/>.  To use it, add the
;; following option at the beginning of your Org file:
;;
;; #+LaTeX_CLASS: brev
;;
;; A certain structure is required in the Org file.  The first level 1
;; heading should have the tag :from:, and contain the sender's name
;; in the heading itself, and the sender's address in the content.
;; Every other level 1 heading is a separate letter, with the
;; recipient's name in the heading and the recipient's address in the
;; content.  The text of the letter starts with a level 2 heading,
;; which becomes the \opening phrase, and concludes with a second
;; level 2 heading with the tag :closing:, which contains the closing
;; phrase in the heading and the signature in the content.

;;; Code:

(require 'org-latex)
(require 'org-beamer)

(eval-when-compile (require 'cl))

(aput 'org-export-latex-classes
      "brev"
      '("\\documentclass[english,british]{brev}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
"
	org-brev-sectioning))

(defvar org-brev--recipient-address-open nil
  "Non-nil if we need to close the recipient address.
\(Internal variable.)")
(make-variable-buffer-local 'org-brev--recipient-address-open)

(defun org-brev-after-initial-vars ()
  (when (string-match "\\\\documentclass\\(\\[[^][]*?\\]\\)?{brev}"
		      org-export-latex-header)
    (setq org-brev--recipient-address-open nil)))

(add-hook 'org-export-latex-after-initial-vars-hook
	  'org-brev-after-initial-vars)

(defun org-brev-sectioning (level text)
  (destructuring-bind (tags heading)
      (with-temp-buffer
	(insert "* " text)
	(list (org-get-tags) (org-get-heading :no-tags)))
    (cond
     ((eql level 1)
      (cond
       ((member "from" tags)
	(let ((in "\\name{%s}\n\\address{")
	      (out (org-add-props (copy-sequence "}")
		       '(org-insert-hook org-brev-unparagraph))))
	  (list heading in out in out)))
       (t
	(let ((in "\\begin{letter}{%s \\\\")
	      (out "\\end{letter}"))
	  (setq org-brev--recipient-address-open t)
	  (list heading in out in out)))))
     ((eql level 2)
      (when org-brev--recipient-address-open
	(org-brev-unparagraph)
	(insert "}\n")
	(setq org-brev--recipient-address-open nil))
      (cond
       ((member "closing" tags)
	;; This is a bit tricky, since the \closing text is in
	;; the heading, but we need \signature to be _before_
	;; \closing.
	(let ((in (concat "\\signature{"))
	      (out (org-add-props
		       (concat "}\n"
			       "\\closing{"
			       heading
			       "}")
		       '(org-insert-hook org-brev-unparagraph))))
	  (list text in out in out)))
       (t
	(let ((in (concat "\\opening{%s}"))
	      (out ""))
	  (list text in out in out))))))))

(defun org-brev-unparagraph ()
  (save-excursion
    (let ((end (point))
	  (beg (search-backward-regexp "^\\\\label{.*}\n\n")))
      (save-restriction
	(narrow-to-region beg end)
	(replace-match "")
	(goto-char (point-min))
	;; Replace all newlines not at the end of the section with two
	;; backslashes.
	(while (search-forward-regexp "\\(\n\\)." (1- (point-max)) t)
	  (replace-match "\\\\" nil t nil 1))))))

(provide 'org-brev)
;;; org-brev.el ends here
