
(setq *inform7-parse-stack* nil)

(setq *inform7-parse-doc* "\"Satisfaction\" by Fredrik Appelberg
The story headline is \"A Short Story About Inevitability\"
The story genre is \"Historical\"


    Include Case Management by Emily Short.
Include Basic Screen Effects by Emily Short.
Include Conversation Framework by Eric Eve.
Include Conversation Suggestions by Eric Eve.

Use no scoring.

Part - Mechanics

Chapter - Switching POV

Switching is an action applying to nothing. Understand \"switch\" as switching.

Carry out switching when the player is Edward: now the player is Roger. Carry out switching when the player is Roger: now the player is Edward. 

Report switching: say \"You are now [the player].\"   ")

(defun inform7-indent-class (str)
  "Calculate the class name used for indentation. STR is a string
  of whitespace."
  (let ((i (+ (count ?\  str)
	      (* (count ?\t str) 4))))
    (concat "i7-indent-" (int-to-string (/ i 4)))))

(defmacro inform7-if-parsing (i e1 e2)
  `(if (eq ,i (car *inform7-parse-stack*))
       (progn (pop *inform7-parse-stack*)
	      ,e1)
     ,e2))

(setq *inform7-tokens*
      (macrolet ((begins (r i e) `(cons ,r (lambda (s) (push ',i *inform7-parse-stack*) ,e)))
		 (produces (r e) `(cons ,r (lambda (s) ,e)))
		 (toggles (r i e1 e2) `(cons ,r (lambda (s) (inform7-if-parsing ',i ,e2 (progn (push ',i *inform7-parse-stack*) ,e1)))))
		 (ends (r i e1 &optional e2) `(cons ,r (lambda (s) (inform7-if-parsing ',i ,e1 ,e2)))))
	(list 
	 (begins   "^[\t ]+" indent  (concat "<div class=\"" (inform7-indent-class s) "\">"))
	 (produces "\\(Volume\\|Book\\|Part\\|Chapter\\|Section\\|Table\\).*?$" (concat "<div class=\"i7-heading\">" s "</div>"))
	 (ends     "$"       indent  "</div>\n" "<br/>\n")
	 (toggles  "\""      quote   "<span class=\"i7-quote\">\"" "\"</span>")
	 (begins   "\\["     bracket "<span class=\"i7-bracket\">[")
	 (ends     "\\]"     bracket "]</span>")
	 (begins   "^."      indent  (concat "<div>" s)))))

(defun inform7-next-token (str start)
  (when-let (m (remove-if-not #'identity
			      (mapcar (lambda (tok)
					(when-let (beg (string-match (car tok) str start))
					  (list beg (match-end 0) (cdr tok))))
				      *inform7-tokens*)))
    (reduce (lambda (a b) (if (< (car a) (car b)) a b)) m)))

(defun inform7-htmlize ()
  (interactive)
  (let ((*inform7-parse-stack* nil)
	(buf (generate-new-buffer "*inform7-htmlize*")))
    (switch-to-buffer buf)
    (nxml-mode)
    (insert (apply 'concat
		   (loop for start = 0 then (if (= start end) (1+ start) end) ;; handle zero-length matches
			 while (< start (length *inform7-parse-doc*))
			 for m = (inform7-next-token *inform7-parse-doc* start)
			 while m
			 for beg = (first m)
			 for end = (second m)
			 append (list 
				 (substring *inform7-parse-doc* start beg)
				 (apply (third m) (list (substring  *inform7-parse-doc* beg end)))))))))

(provide 'inform-htmlizer)