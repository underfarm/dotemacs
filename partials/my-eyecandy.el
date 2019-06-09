
;; Eyecandy
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))


;; (set-face-attribute 'default nil :font "Anonymous Pro" :height 120)
(set-face-attribute 'default nil :font "Consolas" :height 120)

(use-package all-the-icons
  :defer 5
  :config
  (use-package all-the-icons-dired
    :config
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)))


(setq-default prettify-symbols-alist '(("lambda" . ?λ)
				       ("[ ]" .  "☐")
				       ("[X]" . "☑")
				       ("[-]" . "❍" ) 
                                       ("delta" . ?Δ)
                                       ("gamma" . ?Γ)
                                       ("phi" . ?φ)
                                       ("psi" . ?ψ)))

;; My custom modeline
(setq-default mode-line-format
              '((:eval
                 (cond
                  (buffer-read-only
                   (propertize " REO "
                               'face '(:foreground "red" :weight bold)
                               'help-echo "buffer is read-only!!!"))
		              ((buffer-modified-p)
                   (propertize " MOD "
                               'face '(:foreground "orange")))
		              ((equal (buffer-modified-p) nil)
		               (propertize " --- "
				                       ;; 'face '(:foreground "orange")
				                       'help-echo "buffer modified."))))
		            mode-line-buffer-identification
		            " [%l,%c] "
		            " (%m) "
		            (vc-mode vc-mode)
		            ))



(provide 'my-eyecandy)
