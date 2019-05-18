;; ob-powershell.el --- org-babel functions for powershell evaluation

;; Authors: Chris Bilson
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;;; Commentary:

;; Org-Babel support for evaluating powershell source code.


;;; Code:
(require 'ob)
(require 'ob-core)
(eval-when-compile (require 'cl))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("powershell" . "ps1"))

(defvar org-babel-default-header-args:powershell '())


(setq powproc (get-buffer-process "*powershell-babel*"))

;; Expertimental TODO
(defun powershell-filter-process (proc string)
(while (progn
	 (with-current-buffer "test"
	   (insert string))
	 (accept-process-output))))


(setq buffstirng (with-current-buffer "test"
		   (buffer-substring-no-properties (point-min) (point-max))))

(cdr (s-split "," buffstirng))
	 
  (setq original-filter (internal-default-process-filter))
  (setq my-powershell-prompt 


(process-filter (get-buffer-process "*powershell-babel*"))
(set-process-filter powproc 'powershell-filter-process)

(send-string powproc "'test' | gm | % { [string]::Format(\"{0},{1},{2}\", $_.name, $_.membertype,$_.definition)} \n"
(send-string powproc "\n")




     

  )

(defun org-babel-expand-body:powershell (body params)
  body)

(defun powershell-proc-buff-contains-prompt ()
  "Check if buffer contains powershell prompt"
  (with-current-buffer "*powershell-babel*"
    (goto-char (point-min))
    (search-forward-regexp "PS.*>" nil t)))

(defun evalute-babel-ps-buffer (timeout proc-buffer-name)
  (while (eq (powershell-proc-buff-contains-prompt) nil)
    (sleep-for timeout))
  (with-current-buffer proc-buffer-name
    (goto-char (point-min))
    (next-line)
    (let ((buffpoint (point)))
      (buffer-substring-no-properties buffpoint (point-max)))))

; Experimental - using this for now.
 (defun org-babel-execute:powershell (body params)
   "Execute a block of Powershell code with Babel.
This function is called by `org-babel-execute-src-block'."
   (let* ((proc (org-babel-powershell-initiate-session))
	  (proc-buffer "*powershell-babel*")
	  (in-file (org-babel-temp-file "powershell-" ".ps1"))
          (cmdline-filebased (format ". %s \n" in-file))
	  (src-body (org-babel-expand-body:powershell body))
	  (session (or (cdr (assq :session params))
		       nil))
	  (timeout (or (cdr (assq :timeout params))
		       1)))
     (with-current-buffer proc-buffer (erase-buffer))
     ;; with temporary file
     (if (string= "none" session)
	 (progn
	   (with-temp-file in-file
	     (insert src-body))
	   (send-string proc cmdline-filebased)
	   (evalute-babel-ps-buffer timeout proc-buffer))
       ;; with session - not file
       (progn
	 (send-string proc src-body))
       (evalute-babel-ps-buffer timeout proc-buffer))))

;; Works
;; Should i use a timer instead - do i need to?
;; (defun org-babel-execute:powershell (body params)
;;   "Execute a block of Powershell code with Babel.
;; This function is called by `org-babel-execute-src-block'."
;;   (let* ((in-file (org-babel-temp-file "powershell-" ".ps1"))
;;          (cmdline-filebased (or (cdr (assq :cmdline-filebased params))
;; 		      (format ". %s \n" in-file)))
;; 	 (cmd (or (cdr (assq :cmd params))
;; 		  "powershell"))
;; 	 (proc (if (get-buffer "*powershell-babel*")
;; 		   (progn
;; 		     ;; (message "powershell-buffer exists. Deleting previous information.")
;; 		     (with-current-buffer "*powershell-babel*"
;; 		       (erase-buffer))
;; 		     (get-process "powershell"))
;; 		 (progn
;; 		   ;; (message "buffer does not exist - creating new buffer")
;; 		   (org-babel-powershell-initiate-session)))))
;;     (with-temp-file in-file
;;       (insert (org-babel-expand-body:powershell body params)))
;;     (send-string proc cmdline-filebased)
;;     (while (eq (powershell-proc-buff-contains-prompt) nil)
;;       (sleep-for 0.3))
;;     (with-current-buffer "*powershell-babel*"
;;       (goto-char (point-min))
;;       (next-line)
;;       (let ((buffpoint (point)))
;; 	(buffer-substring-no-properties buffpoint (point-max))))))


;; Ulrik: I think we can invoke-command with -filepath and a localhost -session.
;; TODO: I think I *can* support sessions in powershell and really want to...
(defun org-babel-prep-session:powershell (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  ;;TODO: if params then fill out, else default
  (start-process "powershell" "*powershell-babel*" "powershell" "-NoLogo"))

(defun org-babel-variable-assignments:powershell (params)
  "Return list of powershell statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (org-babel-powershell--var-to-powershell (cdr pair) (car pair)))
   (mapcar #'cdr (org-babel--get-vars params))))

;; helper functions
(defvar org-babel-powershell--lvl 0)

(defun org-babel-powershell--var-to-powershell (var &optional varn)
  "Convert an elisp value to a powershell variable.
The elisp value, VAR, is converted to a string of powershell source code
specifying a var of the same value."
  (if varn
      (let ((org-babel-powershell--lvl 0) (lvar (listp var)) prefix)
        (concat "$" (symbol-name varn) "=" (when lvar "\n")
                (org-babel-powershell--var-to-powershell var)
                ";\n"))
    (let ((prefix (make-string (* 2 org-babel-powershell--lvl) ?\ )))
      (concat prefix
              (if (listp var)
                  (let ((org-babel-powershell--lvl (1+ org-babel-powershell--lvl)))
                    (concat "[\n"
                            (mapconcat #'org-babel-powershell--var-to-powershell var "")
                            prefix "]"))
                (format "${%s}" var))
              (unless (zerop org-babel-powershell--lvl) ",\n")))))

(defvar org-babel-powershell-buffers '(:default . nil))

(defun org-babel-powershell-initiate-session (&optional session params)
  "If powershell process is not running - then start process. Returns the powershell process"
  (unless (get-buffer-process "*powershell-babel*")
    (start-process "powershell" "*powershell-babel*" "powershell" "-NoLogo"))
(get-buffer-process "*powershell-babel*"))


(defvar org-babel-powershell-preface nil)

(defun org-babel-powershell-evaluate (session ibody &optional result-type result-params)
  "Pass BODY to the Powershell process in SESSION.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY, as elisp."
  (when session (error "Sessions are not supported for Powershell"))
  (let* ((body (concat org-babel-powershell-preface ibody))
         (out-file (org-babel-temp-file "powershell-"))
         (tmp-babel-file (org-babel-process-file-name
                          out-file 'noquote))
         (in-file (org-babel-temp-file "powershell-"))
         (command (format "%s -File '%s'" org-babel-powershell-command in-file)))

    (with-temp-file in-file
      (insert body))

    (message "body: %s" body)
    (message "in-file: %s" in-file)
    (message "out-file: %s" out-file)
    (message "command: %s" command)

    (let ((results
           (case result-type
             (output
              (with-temp-file out-file
                (insert
                 (org-babel-eval org-babel-powershell-command body))
                (buffer-string)))
             (value
              (message "evaliing now...")
              (org-babel-eval command body)))))
      (when results
        (org-babel-result-cond result-params
          (org-babel-eval-read-file out-file)
          (org-babel-import-elisp-from-file out-file '(16)))))))

(provide 'ob-powershell)
;;; ob-powershell.el ends here
