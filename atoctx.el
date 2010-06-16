;; atoctx.el
;;
;; Converts some LaTeX syntax to ConTeXt
;;
;;
;; Copyright Michael Murphy 2010.
;; Distributed under the GPL. See LICENSE.txt for details.
;;
;; Known bugs (a.k.a. TODO):
;; * does not handle the 'split' environment in equations

(defun convert-buffer-to-context ()
  "Converts the whole buffer to ConTeXt syntax"
  (interactive "*")
  (convert-cite-to-context (point-min) (point-max))
  (convert-emph-to-context (point-min) (point-max))
  (convert-footnote-to-context (point-min) (point-max))
  (convert-enumerate-to-context (point-min) (point-max))
  (convert-amsmath-to-context (point-min) (point-max))
  (convert-figure-to-context (point-min) (point-max))
  )

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

(defun convert-amsmath-to-context (start end)
  "Convert all AMSTeX environments in region to ConTeXt syntax."
  (interactive "*r")
  (convert-equation-to-context start end)
  (convert-align-to-context start end)
  (convert-gather-to-context start end)
  )

(defun convert-equation-to-context (start end)
  "Converts equation commands from AMSTeX to ConTeXt."
  (interactive "*r")
  (convert-amsmath-environ-to-context start end "equation")
  )

(defun convert-align-to-context (start end)
  "Converts align commands from AMSTeX to ConTeXt."
  (interactive "*r")
  (convert-amsmath-environ-to-context start end "align")
  )

(defun convert-gather-to-context (start end)
  "Converts gather commands from AMSTeX to ConTeXt."
  (interactive "*r")
  (convert-amsmath-environ-to-context start end "gather")
  )

(defun convert-amsmath-environ-to-context (start end environ)
  "Convert amsmath environment to ConTeXt."
  (push-mark)
  (save-restriction
    (narrow-to-region start end)
    (delete-trailing-whitespace)
    (goto-char (point-min))
    (while (re-search-forward (concat "\\\\begin{" environ "}") nil t)
      (let (lstart lend lmid n)
	(cond
	 ((string= environ "align") (setq n 2))
	 ((string= environ "equation") (setq n 2))
	 ((string= environ "gather") (setq n 1)))
	(replace-match (concat "\\\\placeformula\n\\\\startformula\\\\startalign[n=" (number-to-string n) "]"))
	;; save point
	(setq lstart (point))
	;; look for the closing part
	(re-search-forward (concat "\\\\end{" environ "}") nil t)
	(replace-match "\\\\stopalign\\\\stopformula")
	(setq lend (match-beginning 0))
	(goto-char lstart)
	(while (re-search-forward "\\\\\\\\" lend t)
	  (replace-match "")
	  (setq lmid (point))
	  (convert-eqline-to-context lstart lmid n)
	  (setq lstart lmid)
	  nil)
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

(defun convert-figure-to-context (start end)
  "Convert figure environment to ConTeXt."
  (interactive "*r")
  (push-mark)
  (save-restriction
    (narrow-to-region start end)
    (delete-trailing-whitespace)
    (goto-char (point-min))
    (let (allfigures)
      (while (re-search-forward "\\\\begin{figure}\\(\\[.*\\]\\)?" nil t)
	(let (fstart fend label caption figure)
	  (replace-match "\\\\placefigure[top]")
	  ;; save point
	  (setq fstart (point))
	  ;; look for the closing part
	  (re-search-forward "\\\\end{figure}" nil t)
	  (replace-match "")
	  (setq fend (match-beginning 0))
	  (goto-char fstart)
	  ;; look for a label
	  (if (re-search-forward "\\\\label{\\(.*?\\)}" fend t)
	      ;; eat the label
	      (let (p1 p2)
		(setq p1 (match-beginning 0))
		(kill-backward-chars 1)
		(setq p2 (point))
		(skip-chars-backward "^{")
		(setq label
		      (concat "[" (buffer-substring-no-properties (point) p2) "]"))
		(kill-region p1 p2)))
	  (goto-char fstart)
	  (insert label)
	  (setq fstart (point))
	  ;; look for a caption, possibly over more than one line
	  (if (re-search-forward "\\\\caption{" fend t)
	      ;; eat the caption
	      (let (p1 p2 p3)
		(setq p1 (match-beginning 0))
		(setq p2 (point))
		;; look for the brace that closes \caption{}
		(setq p3 (TeX-find-closing-brace))
		(setq caption (buffer-substring-no-properties p2 p3))
		(kill-region p1 p3)))
	  (goto-char fstart)
	  (insert (concat "\n{" caption))
	  ;; do some formatting
	  (fill-region fstart (point))
	  (setq fstart (point))
	  ;; look for figure insertion
	  (if (re-search-forward "\\\\includegraphics{\\(.*?\\)}" fend t)
	      ;; eat the caption
	      (let (p1 p2)
		(setq p1 (match-beginning 0))
		(kill-backward-chars 1)
		(setq p2 (point))
		(skip-chars-backward "^{")
		(setq figure (buffer-substring-no-properties (point) p2))
		(kill-region p1 p2)
		;; add to list of global figures for this buffer
		(setq allfigures (cons figure allfigures))
		(goto-char fstart)
		(insert (concat "{\\externalfigure[" figure "]}"))))
	  ;; eat newlines
	  (while (looking-at "\\( \\)*\n[ \n]+") (kill-line) nil)
	  )
	nil)
      (goto-char (point-min))
      (setq allfigures (reverse allfigures))
      (dolist (fig allfigures)
	(insert (concat "\\useexternalfigure[" fig "]\n")))
      )
    )
  )