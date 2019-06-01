
(use-package evil
  :ensure t
  :init
  (setq evil-want-Y-yank-to-eol t)
  :config
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes)
	evil-auto-indent t)
  (evil-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :config
  (setq-default evil-escape-key-sequence "fd")
  (evil-escape-mode))

(use-package restart-emacs)

(use-package general
  :config
  (general-evil-setup)
  (general-define-key :prefix "SPC" :states '(normal)
		      "<SPC>" 'action-key
		      "," 'ubf|eshell-generate
		      "a" 'org-agenda
		      "bm" 'counsel-bookmark
		      "bb" 'switch-to-buffer
		      "bc" 'ubf|my-recompile
		      "bd" 'byte-recompile-directory
		      "c" 'evilnc-comment-or-uncomment-lines
		      "dw" 'delete-whitespace-rectangle'
		      "ff" 'counsel-find-file
		      "edf" 'edebug-defun
		      "ee" 'eval
		      "ef" 'eval-defun
		      "fed" 'ubf|find-file
		      "fg" 'counsel-git-grep
		      "gs" 'magit-status
		      "sa" 'counsel-ag
		      "fb" 'counsel-bookmark
		      "ha" 'apropos
		      "hf" 'counsel-describe-function
		      "hm" 'describe-mode
		      "hk" 'describe-key'
		      "hv" 'counsel-describe-variable
		      "pa" 'projectile-ag
		      "pf" 'projectile-find-file
		      "pa" 'counsel-ag
		      "ps" 'projectile-switch-project
		      "qr" 'restart-emacs
		      "rr" 'copy-to-register
		      "re" 'recentf-open-files
		      "rp" 'insert-register
		      "rb" 'revert-buffer-no-confirm
		      "sa" 'swiper-all
		      "sb" 'bookmark-set
		      "ss" 'swiper
		      "sd" 'counsel-git-grep               ; search directory
		      "ts" 'cycle-spacing
		      "tw" 'whitespace-trailing
		      "u" 'browse-url
		      "w" 'ace-window
		      "qr" 'restart-emacs)
  ;; Visual keybindings - VIM
  (general-define-key
   :prefix "SPC"
   :states '(visual)
   "<SPC>" 'action-key
   "rr" 'copy-to-register
   "a" 'align-regexp
   "c" 'comment-dwim
   "ts" 'cycle-spacing
   "ir" 'indent-region
   "u" 'browse-url
   "e" 'eval-region-or-buffer))


(provide 'my-evil)
