
(defun ubf|find-file ()
  (interactive)
  (find-file user-init-file))


(defun ubf|eshell-switch ()
  (interactive)
  (if(equal (buffer-name) "*eshell*")
      (switch-to-buffer (other-buffer (current-buffer) 1))
    (eshell)))

(defun ubf|my-recompile ()
  (interactive)
  (mapc (lambda (name)
          (byte-compile-file name))
	(directory-files "~/.emacs.d/partials/" t "\\.el$")))

(defun ubf|suround-word (&optional first-char second-char)
  "example of using `bounds-of-thing-at-point'"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
	 (begining (car bounds))
	 (end (cdr bounds))
	 (first-char-to-insert (format "\%s" first-char))
	 (second-char-to-insert (if second-char
				    (format "\%s" second-char)
				  (format "\%s" first-char))))
    (save-excursion
      (goto-char begining)
      (insert first-char-to-insert)
      (goto-char (+ 1 end))
      (insert second-char-to-insert))))

(defun ubf|get-10-random ()
  "Stolen and modified from xah-insert-random-string. Returns 10 random ish characters"
  (interactive)
  (let* ((charset "#!%()=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         (baseCount (length charset)))
    (cl-loop repeat 10
	     concat (char-to-string (elt charset (random baseCount))))))

;; (ubf|dired-do-compress-to '("C:/users/ufarmen/playground/test.wtf") "C:/users/ufarmen/playground/iamSOOOOtestinghtis.7z") 

(defun ubf|dired-do-compress-to (&optional files destination)
  "Compress selected files and directories to an archive.
Prompt for the archive file name."
  (interactive)
  (let* ((in-files (or files
		       (dired-get-marked-files nil nil nil nil t)))
	 (out-file (or destination
		       (expand-file-name (read-file-name "Compress to: "))))
	 (password (ubf|get-10-random))
	 (files-to-string (s-replace-regexp "/" "\\\\" (mapconcat (lambda (x) x) in-files " ")))) ;; Windows path
    (get-buffer-create "*7z*")
    (with-current-buffer "*7z*"
      (goto-char (point-max))
      (insert "\n")
      (insert (format "Archive: %s, pwd: %s" out-file password)))
    (shell-command (format "7z.exe a %s -p%s %s" out-file password files-to-string))))

(provide 'my-functions)

