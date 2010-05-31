(defun convert-cite-to-context (start end)
  "Converts cite commands from LaTeX to ConTeXt."
  (interactive "*r")
  (push-mark)
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\cite{\\(\\(.\\|\n\\)*?\\)}" nil t) 
      (replace-match "\\\\cite[\\1]") nil)
    (goto-char (point-min))
    (while (re-search-forward "\\\\citep{\\(\\(.\\|\n\\)*?\\)}" nil t) 
      (replace-match "\\\\cite[\\1]") nil)
    (goto-char (point-min))
    (while (re-search-forward "\\\\citet{\\(\\(.\\|\n\\)*?\\)}" nil t) 
      (replace-match "\\\\cite[authoryear][\\1]") nil)
    )
  )

(defun convert-emph-to-context (start end)
  "Converts emph commands from LaTeX to ConTeXt."
  (interactive "*r")
  (push-mark)
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\emph{\\(\\(.\\|\n\\)*?\\)}" nil t) 
      (replace-match "{\\\\em \\1}") nil)
    )
  )

(defun convert-footnote-to-context (start end)
  "Converts footnote commands from LaTeX to ConTeXt."
  (interactive "*r")
  (push-mark)
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\footnotemark" nil t)
      (let (fnlab)
	(setq fnlab (read-from-minibuffer
		     "Give this footnote a label: "))
	(replace-match (concat "\\\\note[" fnlab "]")) 
	(if (re-search-forward "\\\\footnotetext" nil t)
	    (replace-match (concat "\\\\footnotetext[" fnlab "]")))
	)
      )
    )
  )

(defun convert-enumerate-to-context (start end)
  "Converts enumerate commands from LaTeX to ConTeXt."
  (interactive "*r")
  (push-mark)
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\\\begin{enumerate}" nil t) 
      (replace-match "\\\\startitemize[packed,inmargin,joinedup]") nil)
    (while (re-search-forward "\\\\end{enumerate}" nil t) 
      (replace-match "\\\\stopitemize") nil)
    )
  )

(defun convert-equation-to-context (start end)
  "Converts equation commands from AMSTeX to ConTeXt."
  (interactive "*r")
  (convert-amsmath-to-context start end "equation")
  )

(defun convert-align-to-context (start end)
  "Converts align commands from AMSTeX to ConTeXt."
  (interactive "*r")
  (convert-amsmath-to-context start end "align")
  )

(defun convert-gather-to-context (start end)
  "Converts gather commands from AMSTeX to ConTeXt."
  (interactive "*r")
  (convert-amsmath-to-context start end "gather")
  )

(defun convert-amsmath-to-context (start end environ)
  (push-mark)
  (save-restriction
    (narrow-to-region start end)
    (delete-trailing-whitespace)
    (goto-char (point-min))
    (while (re-search-forward (concat "\\\\begin{" environ "}") nil t)
      ;; save point
      (let (lstart lend n)
	(cond 
	 ((string= environ "align") (setq n 2))
	 ((string= environ "equation") (setq n 2))
	 ((string= environ "gather") (setq n 1)))
	(replace-match (concat "\\\\placeformula\n\\\\startformula\\\\startalign[n=" (number-to-string n) "]"))
	(setq lstart (point))
	(while (re-search-forward "\\\\\\\\" nil t)
	  (replace-match "")
	  (setq lend (point))
	  (convert-eqline-to-context lstart lend n)
	  (setq lstart lend)
	  nil)
	(setq lstart (point))
	(re-search-forward (concat "\\\\end{" environ "}") nil t)
	(replace-match "\\\\stopalign\\\\stopformula")
	(setq lend (match-beginning 0))
	(convert-eqline-to-context lstart lend n)
	)
      nil)
    )
  )

(defun convert-eqline-to-context (lstart lend n)
  (save-restriction
    (narrow-to-region lstart lend)
    (goto-char (point-min))
    ;; eat newlines
    (while (looking-at "[:space:]*\n") (kill-line) nil)
    (insert "\\NC")
    ;; look for a label
    (let ((label "[+]") p1 p2)
      (if (re-search-forward "\\\\label{\\(.*?\\)}" nil t)
	  ;; eat the label
	  (progn 
	    (setq p1 (match-beginning 0)) 
	    (kill-backward-chars 1) 
	    (setq p2 (point))
	    (skip-chars-backward "^{")
	    (setq label 
		  (concat "[" (buffer-substring-no-properties (point) p2) "]"))
	    (kill-region p1 p2)))
      ;; look for nonumber
      (goto-char (point-min))
      (if (re-search-forward "\\\\nonumber" nil t)
	  ;; eat it, and set label to nothing
	  (progn (replace-match "") (setq label "")))
      ;; if we are just gathering, just use a single column
      (if (> n 1) 
	  (progn (goto-char (point-min))
		 (if (re-search-forward "&" nil t)
		     (replace-match "\\\\NC")
		   (if (re-search-forward "=\\|\\\\equiv" nil t) (replace-match "\\\\NC =")))))
      ;; put NR at nil with label (if any)
      (goto-char (point-max))
      (insert (concat "\\NR" label "\n"))) ;; end let
    )
  )
