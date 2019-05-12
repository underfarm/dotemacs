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

(defun org-babel-expand-body:powershell (body params)
  body)

(defun buffer-contains-substring (string)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun powershell-proc-buff-contains-prompt ()
  "Searches for powershell prompt in buffer"
(with-current-buffer "*powershell-babel*"
			    (goto-char (point-min))
			    (search-forward-regexp "PS.*>")))


(defun org-babel-execute:powershell (body params)
  "Execute a block of Powershell code with Babel.
This function is called by `org-babel-execute-src-block'."
    (let* ((in-file (org-babel-temp-file "powershell-" ".ps1"))
           (cmdline (or (cdr (assq :cmdline params))
			(format ". %s \n" in-file)))
		    (cmd (or (cdr (assq :cmd params))
			     "powershell"))
		    (proc (if (get-buffer "*powershell-babel*")
			      (progn
				(message "buffer exists!")
			      (get-process "powershell"))
			    (progn
			      (message "buffer does not exist!")
			      (org-babel-powershell-initiate-session)))))
      (with-temp-file in-file
(insert (org-babel-expand-body:powershell body params)))
      (send-string proc cmdline)
      ;;
      (setq powershell-timer nil)
      (let ((timer (timer-create)))
	(timer-set-time timer (current-time) 0.1)
	(timer-set-function
	 timer
	 ;; This depends on lexical binding
	 #'(lambda () (when (powershell-proc-buff-contains-prompt) 
			     (powershell-timer-cancel)
			     ((with-current-buffer "*powershell-babel*"
				(erase-buffer))))))
	(timer-activate timer)
	(setq powershell-timer timer))
      ;;
      (with-current-buffer "*powershell-babel*"
	(buffer-substring-no-properties (point-min) (point-max)))))


(defun powershell-timer-cancel ()
  "Cancel the current Piziak-style timer"
  (cancel-timer powershell-timer)
  (setq powershell-timer nil))

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
  "test"
  (start-process "powershell" "*powershell-babel*" "powershell" "-NoLogo"))

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
