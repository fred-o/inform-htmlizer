
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

(setq *inform7-tokens* 
      '(("^\\s*" . (lambda (s) (concat "<div class=\"indent\">")))
	("^\\(Volume\\|Book\\|Part\\|Chapter\\|Section\\|Table\\).*?$" . (lambda (s) (concat "<div class=\"heading\">" s "</div>")))
	("$" . (lambda (s) "</div>\n"))
	("\"" . (lambda (s) (if (eq 'quote (car *inform7-parse-stack*))
				(progn (pop *inform7-parse-stack*)
				       "</span>\"")
			      (progn (push 'quote *inform7-parse-stack*)
				     "<span class=\"quote\">\""))))
	("\\[" . (lambda (s) 
		  (push 'bracket *inform7-parse-stack*)
		  "<span class=\"bracket\">["))
	("\\]" . (lambda (s) 
		  (pop *inform7-parse-stack*)
		  "]</span>"))))

(defun inform7-next-token (str start)
  (when-let (m (remove-if-not #'identity
			      (mapcar (lambda (tok)
					(when-let (beg (string-match (car tok) str start))
					  (list beg (match-end 0) (cdr tok))))
				      *inform7-tokens*)))
    (reduce (lambda (a b) (if (< (car a) (car b)) a b)) m)))

(defun inform7-htmlize ()
  (loop for start = 0 then (if (= start end) (1+ start) end) ;; handle zero-length matches
	while (< start (length *inform7-parse-doc*))
	for m = (inform7-next-token *inform7-parse-doc* start)
	while m
	for beg = (first m)
	for end = (second m)
	append (list 
		(substring *inform7-parse-doc* start beg)
		(apply (third m) (list (substring  *inform7-parse-doc* beg end))))))

(setq res (inform7-htmlize))

(defun inform7-htmlize (str)
  (interactive 
   (list (buffer-substring (region-beginning) (region-end))))


(provide 'inform-htmlizer)