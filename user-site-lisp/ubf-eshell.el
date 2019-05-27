
(defun ubf|eshell-switch (&optional create-new-eshell)
  (interactive)
  (cond
   (create-new-eshell ;; If argument is passed.
    (eshell 'n))
   ((s-contains? "*eshell*" (buffer-name))
    (switch-to-buffer (other-buffer (current-buffer) 1))) ;; If we already are in an eshell-buffer, switch back.
   (t (eshell)))) ;; Default case --> Return to default eshell.
     
(defun ubf--verify-eshelbuffer-exist (eshell-buffers pwd-buffer)
  (if eshell-buffers ;; If no check for buffers --> endless loop!
      (if (-contains? (car eshell-buffers) (car pwd-buffer))
	  (car eshell-buffers)
	(ubf--verify-eshelbuffer-exist (cdr eshell-buffers) pwd-buffer))))


(defun ubf--return-pwd-from-buffer (buffer-names)
  "Returns the pwd of eshell buffer-name. It will be a list with ('Name of buffer' 'buffer-parent-directory)"
  (let* ((paths (mapcar
		 (lambda (x)
		   (cons x (with-current-buffer x
			     (pwd))))
		 buffer-names)))
    (cl-loop for p in paths
	     collect (cons (car p) (last (s-split "/" (cdr p) t))))))

(defun ubf|eshell-generate ()
  "Will loop through open buffers and switch to eshell-buffer if it exists for the parent directory. If it does not exist, create new eshell-buffer in buffer directory"
  (interactive)
  (let* ((buffs (mapcar 'buffer-name (buffer-list)))
	 (members (remove nil (mapcar #'(lambda (x)
					  (when (s-contains? "*eshell*" x) x))
				      buffs)))
	 (current-name (last (s-split "/" (file-name-directory (buffer-file-name)) t))))
    (setq found (ubf--verify-eshelbuffer-exist (ubf--return-pwd-from-buffer members) current-name))
    (if found
	(switch-to-buffer (car found))
      (ubf|eshell-switch t))))

;; (ubf--verify-eshelbuffer-exist '(("*eshell*" "user-site-lisp"))
;; ==> ("*eshell*" "user-site-lisp")
;; (ubf--return-pwd-from-buffer '("*eshell*"))
;; ==> (("*eshell*" "user-site-lisp"))

(provide 'ubf-eshell)
