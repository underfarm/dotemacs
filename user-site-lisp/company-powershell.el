(require 'cl-lib)
(require 'company)
(require 'ps-ivy)

;; TEST
;; Get-Process
;; 

(defun company-pscommand-backend (command &optional arg &rest ignored)
  (unless (get-buffer-process "*powershell-babel*")
    (ps|powershell-initiate-session))
  (interactive (list 'interactive))
  (setq current-point (point))
  (cl-case command
    (interactive (company-begin-backend 'company-pscommand-backend))
    (post-completion (insert " "))
    (prefix (when (re-search-backward "\\<\\w+-\\w+"))
            (goto-char current-point)
            (match-string 0))
    (candidates (ps|return-company-candidates arg))))

(defun ps|return-company-candidates (arg)
  (let ((res (ps|return-completion-candidates arg
                :output-buffer "*powershell-babel*"
                :process (get-buffer-process ps-proc-buffer-name)
                :variable-definition 'command)))
    res))

(defun ps|company--insert-candidate (candidate)
  (progn
    (message "company backend! %s" company-backend))
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    ;; XXX: Return value we check here is subject to change.
    (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
        (insert candidate))
    (unless (equal company-prefix candidate)
      (if (eq company-backend 'company-pscommand-backend) ;; The (if is custom, to accomodate powershell.
	  (insert candidate)
        (delete-region (- (point) (length company-prefix)) (point))
        (insert candidate)))))


;; INFO - I advice the function in order to accomodarte completion of candidates that does not contain the prefix.
;; IE, i want Get-Process to complete '-ComputerName', and not Get-Process to complete to Get-Process or something that has the same prefix, ie Get-Pro --> Get-Process.
(advice-add 'company--insert-candidate :override #'ps|company--insert-candidate)

(provide 'company-powershell)
