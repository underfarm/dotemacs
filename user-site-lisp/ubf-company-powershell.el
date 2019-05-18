(require 's)

(setq ps-proc-buffer-name "*powershell-babel*")

(progn
  (org-babel-powershell-initiate-session)
  (process-send-string (get-buffer-process "*powershell-babel*")  "\n")
  (process-send-string (get-buffer-process "*powershell-babel*")  "$hantja = 'santja' \n")
  (setq wtf (ubf--get-powershel-completion "$hantja" :process (get-buffer-process "*powershell-babel*") :output-buffer "*powershell-babel*")))


get-santja get-process 


(defun org-babel-powershell-initiate-session (&optional session params)
  "Create the powershell process in <buffer>"
  (when (eq (get-buffer-process "*powershell-babel*") nil)
    (start-process "powershell" "*powershell-babel*" "powershell" "-nologo")))


(defun powershell-proc-buff-contains-prompt (proc-buffer)
  "Check if buffer contains powershell prompt"
  (with-current-buffer proc-buffer
    (goto-char (point-min))
    (search-forward-regexp "PS.*>" nil t)))

(make-powershell-commandstring "Get-Process" :variable-definition 'command)

(cl-defun make-powershell-commandstring (variable &key variable-definition)
  (cond
   ((eq variable-definition 'variable)
    (format "%s | gm | foreach-object { [string]::Format(\"{0} -> {1} -> |> {2}\", $_.name, $_.membertype, $_.definition)} \n"
	    (s-replace-regexp "\\." "" variable))) ; Dont want periods in our variable when sending it to PS.
   ((eq variable-definition 'command)
    (format
     "(get-command %s | Select-String -Pattern \"\\W(?<mycap>-\\w+)\" -AllMatches).Matches.Groups | ? { $_.name -eq 'mycap' } | Select-Object -ExpandProperty \n" variable))))


(cl-defun ubf--get-powershel-completion (variable &key variable-definition process output-buffer)
  (let* ((output output-buffer)
	 (powershell-command-string ""))
    (with-current-buffer output
      (erase-buffer));; clean old entries
    (cond
     ((eq variable-definition 'variable)
      (setq powershell-variable (s-replace-regexp "\\." "" variable)) ;; Dont want periods in our variable when sending it to PS.
      (setq powershell-command-string (format "%s | gm | foreach-object { [string]::Format(\"{0} -> {1} -> |> {2}\", $_.name, $_.membertype, $_.definition)} \n"
					      variable)))
     ((eq variable-definition 'command)
      (setq powershell-command-string (format
				       "(get-command %s | Select-String -Pattern \"\\W(?<mycap>-\\w+)\" -AllMatches).Matches.Groups | ? { $_.name -eq 'mycap' } | Select-Object -ExpandProperty \n" variable))))

    ;; Do the deed - hit me, powershell!
    (send-string process powershell-command-string)
    ;; Wait for the prompt to return to the buffer
    (while (eq (powershell-proc-buff-contains-prompt output) nil)
      (sleep-for 0.01)) 
    (s-split "\n" (with-current-buffer output
		    (goto-char (point-min))
		    (next-line) ;; Dont want the part which contains the prompt.
		    (buffer-substring-no-properties (point) (point-max)))
	     t)))


(defun command-at-p ()
  (interactive)
  (let* ((psvariable (thing-at-point 'symbol t))
	 (proc (get-buffer-process ps-proc-buffer-name))
	 (output-buffer ps-proc-buffer-name)
	 (candidates (ubf--get-powershel-completion psvariable
						    :process proc
						    :variable-definition 'command
						    :output-buffer ps-proc-buffer-name)))
    (ivy-read "Find symbols: " candidates
	      :action (lambda (x)
			(insert x)))))

(defun xx-at-p ()
  (interactive)
  (let* ((psvariable (thing-at-point 'symbol t))
	 (proc (get-buffer-process ps-proc-buffer-name))
	 (output-buffer ps-proc-buffer-name)
	 (candidates (ubf--get-powershel-completion psvariable
						    :process proc
						    :variable-definition 'variable
						    :output-buffer ps-proc-buffer-name)))
    (ivy-read "Find symbols: " candidates
	      :action (lambda (x)
			(let* ((splitted (s-split "->" x))
			       (first (s-trim (nth 0 splitted)))
			       (second (s-trim (nth 1 splitted))))
			  (if (s-contains-p "Method" second)
			      ;; If we have a method, we want to insert parenthesis.
			      (setq ubf-powershell-candidate (format "%s(" first))
			    (setq ubf-powershell-candidate first))
			  (if (looking-back "\\.") ;; Insert period if needed.
			      (insert ubf-powershell-candidate)
			    (insert (format ".%s" ubf-powershell-candidate))))))))

