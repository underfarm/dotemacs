
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

(provide 'my-functions)

