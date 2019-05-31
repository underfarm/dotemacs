
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
	(directory-files "~/.emacs.d/partials/" nil "\\.el$")))


(defun my-get-boundary-and-thing (&optional)
  "example of using `bounds-of-thing-at-point'"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
	 (begining (car bounds))
	 (end (cdr bounds)))
    (save-excursion
      (cond
       ()

       )
      (goto-char begining)
      (insert "\"")
      (goto-char (+ 1 end))
      (insert "\""))))


(provide 'my-functions)

