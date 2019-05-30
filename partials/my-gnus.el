(with-eval-after-load "gnus"


      ;; Use topics per default
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  (setq gnus-visible-headers
        "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus")

  ;; Show the article headers in this order.
  (setq gnus-sorted-header-list
        '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
          "^Subject:" "^Date:" "^Gnus"))

  (setq ;; You need to replace this email address with your own!
   user-mail-address "ulrik.bruun.farmen@gmail.com"
   ;; This tells Gnus to get email from Gmail via IMAP.
   gnus-select-method
   '(nnimap "gmail"
            ;; It could also be imap.googlemail.com if that's your server.
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl))
   ;; This tells Gnus to use the Gmail SMTP server. This
   ;; automatically leaves a copy in the Gmail Sent folder.
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587
   ;; Tell message mode to use SMTP.
   message-send-mail-function 'smtpmail-send-it
   ;; Gmail system labels have the prefix [Gmail], which matches
   ;; the default value of gnus-ignored-newsgroups. That's why we
   ;; redefine it.
   gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
   ;; The agent seems to confuse nnimap, therefore we'll disable it.
   gnus-agent nil
   ;; We don't want local, unencrypted copies of emails we write.
   gnus-message-archive-group nil
   ;; We want to be able to read the emails we wrote.
   gnus-fetch-old-headers t
   gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
   gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
   gnus-group-line-format "%M%S%p%P%5y:%B %G\n";;"%B%(%g%)"
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
   gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
   gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]"
   gnus-sum-thread-tree-false-root ""
   gnus-sum-thread-tree-indent " "
   gnus-sum-thread-tree-leaf-with-other "├► "
   gnus-sum-thread-tree-root ""
   gnus-sum-thread-tree-single-leaf "╰► "
   gnus-sum-thread-tree-vertical "│"
   gnus-article-browse-delete-temp t
   gnus-treat-strip-trailing-blank-lines 'last
   gnus-keep-backlog 'nil
   gnus-summary-display-arrow nil ; Don't show that annoying arrow:
   gnus-mime-display-multipart-related-as-mixed t ; Show more MIME-stuff:
   gnus-auto-select-first nil ; Don't get the first article automatically:
   smiley-style 'medium
   gnus-keep-backlog '0
   )

(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
     (setq gnus-topic-topology '(("Gnus" visible)
                                 (("gmail" visible nil nil))))

;; key of topic is specified in my sample ".gnus.el"
     (setq gnus-topic-alist '(
			      ("gmail" ; the key of topic
			       "nnimap+gmail:INBOX"
			       "nnimap+gmail:[Gmail]/Sent Mail"
			       "nnimap+gmail:[Gmail]/Drafts")
			      ("Gnus")))))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

					; NO 'passive
(setq gnus-use-cache t)

;; {{ press "o" to view all groups
(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

(define-key gnus-group-mode-map
  ;; list all the subscribed groups even they contain zero un-read messages
  (kbd "o") 'my-gnus-group-list-subscribed-groups)
;; }}

;; open attachment
(eval-after-load 'mailcap
  '(progn
     (cond
      ;; on macOS, maybe change mailcap-mime-data?
      ((eq system-type 'darwin))
      ;; on Windows, maybe change mailcap-mime-data?
      ((eq system-type 'windows-nt))
      (t
       ;; Linux, read ~/.mailcap
       (mailcap-parse-mailcaps)))))

;; Tree view for groups.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first message.
;; `gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)


;; Read HTML mail:
;; You need install the command line web browser 'w3m' and Emacs plugin 'w3m'
;; manually. It specify the html render as w3m so my setup works on all versions
;; of Emacs.
;;
;; Since Emacs 24+, a default html rendering engine `shr' is provided:
;;   - It works out of box without any cli program dependency or setup
;;   - It can render html color
;; So below line is optional.

;; Send email through SMTP
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")
;; http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
(setq gnus-use-correct-string-widths nil)

)

(provide 'my-gnus)
