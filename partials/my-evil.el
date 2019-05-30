
(use-package evil
  :ensure t
  :init
  (setq evil-want-Y-yank-to-eol t)
  :config
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))
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
		      "ff" 'counsel-find-file
		      "edf" 'edebug-defun
		      "ee" 'eval
		      "ef" 'eval-defun
		      "fed" 'ubf|find-file
		      "fg" 'counsel-git-grep
		      "sa" 'counsel-ag
		      "fb" 'counsel-bookmark
		      "ha" 'apropos
		      "hf" 'counsel-describe-function
		      "hm" 'describe-mode
		      "hv" 'counsel-describe-variable
		      "pa" 'projectile-ag
		      "pp" 'counsel-projectile
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
		      "u" 'browse-url
		      "w" 'ace-window
		      "qr" 'restart-emacs)
  ;; Visual keybindings - VIM
  (general-define-key
   :prefix "SPC"
   :states '(visual)
   "rr" 'copy-to-register
   "a" 'align-regexp
   "c" 'comment-dwim
   "ts" 'cycle-spacing
   "ir" 'indent-region
   "u" 'browse-url
   "e" 'eval-region-or-buffer))


(provide 'my-evil)
