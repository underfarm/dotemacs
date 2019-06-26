
(when (eq system-type 'gnu/linux)


  (use-package mu4e
    :load-path "/opt/mu-1.2.0/mu4e"
    :config

    (use-package org-mime)
    

    (setq mu4e-mu-binary "/usr/local/bin/mu")
    (setq mu4e-maildir "/home/ufarmen/snap/offlineimap/current/Maildir")
    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)

    (setq mu4e-contexts
	  `( ,(make-mu4e-context
	       :name "Gmail"
	       :match-func (lambda (msg) (when msg
				      (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
	       :vars '(
		       (mu4e-trash-folder . "/Gmail/[Gmail].Trash")
		       (mu4e-refile-folder . "/Gmail/[Gmail].Archive")
		       ))
	     ))

    ;; I have my "default" parameters from Gmail
    (setq mu4e-sent-folder "/Users/ufarmen/Maildir/sent"
	  mu4e-drafts-folder "/[Gmail].Drafts"
	  mu4e-sent-folder   "/[Gmail].Sent Mail"
	  mu4e-sent-messages-behavior 'delete

	  ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
	  user-mail-address "ulrik.bruun.farmen@gmail.com"
	  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
	  smtpmail-auth-credentials
	  '(("smtp.gmail.com" 587 "ulrik.bruun.farmen@gmail.com" nil))
	  smtpmail-default-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-service 587)

    (require 'smtpmail)

    (setq message-send-mail-function 'smtpmail-send-it
	  starttls-use-gnutls t
	  smtpmail-starttls-credentials
	  '(("smtp.gmail.com" 587 nil nil))
	  smtpmail-auth-credentials
	  (expand-file-name "~/.authinfo.gpg")
	  smtpmail-default-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-service 587
	  smtpmail-debug-info t)

    ;; Now I set a list of 
    ;;  (defvar my-mu4e-account-alist
    ;;    '(("Gmail"
    ;; 	 (mu4e-sent-folder "/Gmail/sent")
    ;; 	 (user-mail-address "ulrik.bruun.farmen@gmail.com")
    ;; 	 (smtpmail-smtp-user "ulrik.bruun.farmen@gmail.com")
    ;; 	 (smtpmail-local-domain "gmail.com")
    ;; 	 (smtpmail-default-smtp-server "smtp.gmail.com")
    ;; 	 (smtpmail-smtp-server "smtp.gmail.com")
    ;; 	 (smtpmail-cred-user "ulrik.bruun.farmen@gmail.com")
    ;; 	 (smtpmail-cred-passwd "qezfvtamrkmjshkm")
    ;; 	 (smtpmail-smtp-service 587)
    ;; 	 )
    ;; 	;; Include any other accounts here ...
    ;; 	))

    ;;  (defun my-mu4e-set-account ()
    ;;    "Set the account for composing a message.
    ;; This function is taken from: 
    ;;   https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
    ;;    (let* ((account
    ;; 	      (if mu4e-compose-parent-message
    ;; 		  (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
    ;; 		    (string-match "/\\(.*?\\)/" maildir)
    ;; 		    (match-string 1 maildir)
    ;; 		    (completing-read (format "Compose with account: (%s) "
    ;; 					     (mapconcat #'(lambda (var) (car var))
    ;; 							my-mu4e-account-alist "/"))
    ;; 				     (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
    ;; 				     nil t nil nil (caar my-mu4e-account-alist))))
    ;; 	      (account-vars (cdr (assoc account my-mu4e-account-alist))))
    ;; 	     (if account-vars
    ;; 		 (mapc #'(lambda (var)
    ;; 			   (set (car var) (cadr var)))
    ;; 		       account-vars)
    ;; 	       (error "No email account found"))))
    ;;    (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

    )

  (use-package evil-mu4e
    :after mu4e)

  )


(provide 'my-mail)

