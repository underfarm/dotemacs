(require 's)


(defun ubf|eshell-switch (&optional create-new-eshell)
  (interactive)
  (cond
   (create-new-eshell ;; If argument is passed.
    (eshell 'n))
   ((s-contains? "*eshell*" (buffer-name))
    (switch-to-buffer (other-buffer (current-buffer) 1))) ;; If we already are in an eshell-buffer, switch back.
   (t (eshell)))) ;; Default case --> Return to default eshell.

;; (if (-contains? (car eshell-buffers) (car pwd-buffer))
;; 	  (car eshell-buffers)
;; 	(ubf--verify-eshelbuffer-exist (cdr eshell-buffers) pwd-buffer)))


(defun ubf--return-eshell-buffer-name (eshell-buffers pwd-buffer)
  "Recursive helper function. Cheks if pwd of buffer exists in our list of eshell buffers. Returns ('Name of buffer' 'buffer-parent-directory) if found."
  (if eshell-buffers ;; If no check for buffers --> endless loop!
      (if (-contains? (car eshell-buffers) (car pwd-buffer))
	  (car eshell-buffers)
	(ubf--return-eshell-buffer-name (cdr eshell-buffers) pwd-buffer))))

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
  "Will loop through open buffers and switch to eshell-buffer if it exists for the parent directory. If it does not exist, create new eshell-buffer in the directory of the buffer."
  (interactive)
  (if (s-contains? "*eshell*" (buffer-name))
      (switch-to-buffer (other-buffer (current-buffer) 1)) ;; If we already are in an eshell-buffer, switch back.
    (progn 
      (let* ((buffs (mapcar 'buffer-name (buffer-list)))
	     (eshell-buffers (remove nil (mapcar #'(lambda (x)
						     (when (s-contains? "*eshell*" x) x))
						 buffs)))
	     (current-buffer-name (last (s-split "/" (file-name-directory (buffer-file-name)) t))))
	(setq found (ubf--return-eshell-buffer-name (ubf--return-pwd-from-buffer eshell-buffers) current-buffer-name))
	(if found
	    (switch-to-buffer (car found))
	  (ubf|eshell-switch t))))))

;; (ubf--return-eshell-buffer-name '(("*eshell*" "user-site-lisp"))
;; ==> ("*eshell*" "user-site-lisp")
;; (ubf--return-pwd-from-buffer '("*eshell*"))
;; ==> (("*eshell*" "user-site-lisp"))

;; EyeCandy for Eshell

(setq eshell-prompt-function
      (lambda ()
  	(concat
  	 (propertize "┌─[" 'face					`(:foreground "#ffaf00"))
  	 (propertize (user-login-name) 'face				`(:foreground "#870000"))
  	 (propertize "@" 'face						`(:foreground "#ffaf00"))
  	 (propertize (system-name) 'face				`(:foreground "#870000"))
  	 (propertize "]──[" 'face					`(:foreground "#ffaf00"))
  	 (propertize (format-time-string "%H:%M" (current-time)) 'face	`(:foreground "yellow"))
  	 (propertize "]──[" 'face					`(:foreground "#ffaf00"))
  	 (propertize (concat (eshell/pwd)) 'face			`(:foreground "#a8a8a8"))
  	 (propertize "]\n" 'face					`(:foreground "#ffaf00"))
  	 (propertize "└─>" 'face					`(:foreground "#ffaf00"))
  	 (propertize (if (= (user-uid) 0) " # " " $ ") 'face		`(:foreground "#ffaf00"))
  	 )))


(provide 'my-shell)
