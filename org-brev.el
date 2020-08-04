;;; org-brev.el --- export Org files to LaTeX using the brev class

;; Copyright (C) 2009, 2013, 2014  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Version: 0.2

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
;; at <https://github.com/asgeirn/brev-cls/>.  To use it, add the
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
;; level 2 heading, which contains the closing phrase in the heading
;; and the signature in the content.
;;
;; If you are running Emacs 24.1 or later, install this module by
;; typing M-x package-install-file and specifying this file
;; (org-brev.el).

;;; Code:

(require 'ox-latex)
(require 'ox-beamer)

(eval-when-compile (require 'cl))

(defgroup org-brev nil
  "Options for exporting Org files using the LaTeX `brev' class."
  :group 'org)

(defcustom org-brev-recipient-address-prefix ""
  "Text to add before the recipient address.
This can be used to adapt the output to various kinds of window
envelopes.  For example, if the first few lines of the address are not
visible, try:

\\vspace{2cm} \\\\"
  :type 'string)

;;;###autoload
(eval-after-load "ox-latex"
  ;; Remove any existing "brev" definition.
  '(progn
     (let ((entry (assoc "brev" org-latex-classes)))
       (when entry
	 (setq org-latex-classes
	       (remq entry org-latex-classes)))
       (add-to-list
	'org-latex-classes
	'("brev" "\\documentclass{brev}"
	  ())))
     (org-export-define-derived-backend 'brev 'latex
       :translate-alist '((headline . org-brev-headline))
       ;; Disable title and table of contents
       :options-alist '((:title nil nil "" t)
			(:with-toc nil nil nil t))
       :menu-entry
       '(?b "Export with Brev"
	    ((?L "As LaTeX buffer" org-brev-export-as-latex)
	     (?l "As LaTeX file" org-brev-export-to-latex)
	     (?p "As PDF file" org-brev-export-to-pdf)
	     (?o "As PDF file and open"
		 (lambda (a s v b)
		   (if a (org-brev-export-to-pdf t s v b)
		     (org-open-file (org-brev-export-to-pdf nil s v b))))))))))

;;;###autoload
(defun org-brev-export-as-latex
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'brev "*Org BREV Export*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

;;;###autoload
(defun org-brev-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'brev file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-brev-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'brev file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

(defvar org-brev--recipient-address-open nil
  "Non-nil if we need to close the recipient address.
\(Internal variable.)")
(make-variable-buffer-local 'org-brev--recipient-address-open)

(defvar org-brev--had-opening nil
  "Non-nil if the current letter has already had an opening phrase.
\(Internal variable.)")
(make-variable-buffer-local 'org-brev--had-opening)

(defun org-brev-headline (headline contents info)
  (let ((tags (org-export-get-tags headline info))
	(heading (car (org-element-property :title headline)))
	(level (org-element-property :level headline)))
    (cond
     ((eql level 1)
      (cond
       ((member "from" tags)
	(concat "\\name{" heading "}\n\\address{"
		(replace-regexp-in-string "\n" "\\\\\\\\" contents)
		"}\n"))
       (t
	(setq org-brev--recipient-address-open t)
	(setq org-brev--had-opening nil)
	(concat "\\begin{letter}{" org-brev-recipient-address-prefix
		heading " \\\\"
		(or (and (string-match "\\\\opening" contents)
			 (let ((address (substring contents 0 (match-beginning 0)))
			       (rest (substring contents (match-beginning 0))))
			   (concat
			    (replace-regexp-in-string "\n" "\\\\\\\\" address)
			    "}\n"
			    rest)))
		    ;; No opening?...
		    (replace-regexp-in-string "\n" "\\\\\\\\" contents))
		"\n\\end{letter}"))))
     ((eql level 2)
      (concat
       (when org-brev--recipient-address-open
	 (setq org-brev--recipient-address-open nil)
	 (concat contents "}\n"))
       (cond
	(org-brev--had-opening
	 (concat
	  "\\signature{" contents "}\n"
	  "\\closing{" heading "}\n"))
	(t
	 (setq org-brev--had-opening t)
	 (concat "\\opening{" heading "}\n" contents))))))))

(provide 'org-brev)
;;; org-brev.el ends here
