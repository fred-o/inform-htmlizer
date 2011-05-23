;; Utility functions for producing an HTML-version of Inform7 source
;; code with some syntax hightlighting.
;;
;; Author  : Fredrik Appelberg <fredrik.appelberg@gmail.com>
;; Version : 0.1
;; Licence : Public Domain
;;

(setq *inform7-parse-stack* nil)

(defun inform7-indent-class (str)
  "Calculate the class name used for indentation. STR is a string
  of whitespace."
  (let ((i (+ (count ?\  str)
	      (* (count ?\t str) 4))))
    (concat "i7-indent-" (int-to-string (/ i 4)))))

(defmacro inform7-if-parsing (i e1 e2)
  "Helper macro that executes either E1 or E2 depending on
whether we're currently parsing I"
  `(if (eq ,i (car *inform7-parse-stack*))
       (progn (pop *inform7-parse-stack*)
	      ,e1)
     ,e2))

(setq *inform7-tokens*
      (macrolet ((begins (r i e) `(cons ,r (lambda (s) (push ',i *inform7-parse-stack*) ,e)))
		 (toggles (r i e1 e2) `(cons ,r (lambda (s) (inform7-if-parsing ',i ,e2 (progn (push ',i *inform7-parse-stack*) ,e1)))))
		 (ends (r i e1 &optional e2) `(cons ,r (lambda (s) (inform7-if-parsing ',i ,e1 ,e2)))))
	(list 
	 (begins   "^."      indent  (concat "<div>" s))
	 (ends     "$"       indent  "</div>\n" "<br/>\n")
	 (begins   "^[\t ]+" indent  (concat "<div class=\"" (inform7-indent-class s) "\">"))
	 (begins   "\\(Volume\\|Book\\|Part\\|Chapter\\|Section\\|Table\\).*?$" indent (concat "<div class=\"i7-heading\">" s))
	 (toggles  "\""      quote   "<div class=\"i7-quote\">\"" "\"</div>")
	 (begins   "\\["     bracket "<div class=\"i7-bracket\">[")
	 (ends     "\\]"     bracket "]</div>"))))

(defun inform7-next-token (str start)
  (when-let (m (remove-if-not #'identity
			      (mapcar (lambda (tok)
					(when-let (beg (string-match (car tok) str start))
					  (list beg (match-end 0) (cdr tok))))
				      *inform7-tokens*)))
    (reduce (lambda (a b) (if (< (car a) (car b)) a b)) m)))

(defun inform7-htmlize (input &optional emit-body)
  "Produces an HTML representation of the given Inform7 source
INPUT. If invoked interactively, the current region (if active)
or the entire buffer will be used as INPUT. If EMIT-BODY, or the
universal prefix argument, is true, a HTML preamble will also be
added."
  (interactive 
   (list (if (region-active-p) (buffer-substring (region-beginning) (region-end))
	   (buffer-string))
	 current-prefix-arg))
  (let ((*inform7-parse-stack* nil)
	(buf (generate-new-buffer "*inform7-htmlize*")))
    (switch-to-buffer buf)
    (when emit-body
      (insert "<html>\n<head>\n<link rel=\"stylesheet\" href=\"inform-htmlize.css\" />\n</head>\n<body>\n"))
    (insert "<div class=\"i7-sample\">\n")
    (insert (apply 'concat
		   (loop for start = 0 then (if (= beg end) (1+ end) end) ;; handle zero-length matches
			 while (< start (length input))
			 for m = (inform7-next-token input start)
			 while m
			 for beg = (first m)
			 for end = (second m)
			 append (list 
				 (substring input start beg)
				 (apply (third m) (list (substring input beg end)))))))
    (insert "\n</div>\n")
    (when emit-body
      (insert "</body>\n\</html>"))
    (html-mode)
    (indent-region 0 (buffer-end 1))))

(provide 'inform-htmlizer)