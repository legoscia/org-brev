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
