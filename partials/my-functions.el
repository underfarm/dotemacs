
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

(provide 'my-functions)

