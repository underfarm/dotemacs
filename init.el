
;; Actually, we can win back half of that 0.2s right away with a simple trick:
;; Less GC during startup

(setq package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      visible-bell t
      initial-scratch-message ";; ready\n\n"
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/"))
      inhibit-startup-screen t
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; That sweet fullsize
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(setq use-package-always-ensure t)


(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

(setq user-full-name "Ulrik B. Farmen"
      user-mail-address "ulrik.bruun.farmen@gmail.com")

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p) 

;; Custom elisp
(add-to-list 'load-path "~/.emacs.d/user-site-lisp/")


;; Eyecandy
(load-theme 'material t)

(set-face-attribute 'default nil :font "Anonymous Pro" :height 120)

(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("delta" . ?Δ)
                                       ("gamma" . ?Γ)
                                       ("phi" . ?φ)
                                       ("psi" . ?ψ)))

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text
           global-auto-revert-mode      ; Reverts the buffer if file has changed
           scroll-bar-mode              ; No scroll bars either
           blink-cursor-mode))          ; The blinking cursor gets old
  (funcall mode 0))


(dolist (mode
         '(recentf-mode))
  (funcall mode 1))


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


;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;; Disk space is cheap. Save lots.

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

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
  (general-evil-setup t)
  (general-define-key :prefix "SPC" :states '(normal)
   "," 'ubf|eshell-switch
   "a" 'org-agenda
   "bm" 'counsel-bookmark
   "bb" 'switch-to-buffer
   "c" 'evilnc-comment-or-uncomment-lines
   "ff" 'counsel-find-file
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
   "rp" 'insert-register
   "rb" 'revert-buffer-no-confirm
   "sa" 'swiper-all
   "sb" 'bookmark-set
   "ss" 'swiper
   "sd" 'counsel-git-grep               ; search directory
   "ts" 'cycle-spacing
   "u" 'browse-url
   "w" 'switch-window
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



   (require 'ob-powershell)


;; Powershell
  (use-package powershell
    :mode ("\\.ps1\\'" . powershell-mode)
    :config
    (require 'ob-powershell)

    )
    (add-hook 'powershell-mode-hook #'lsp)

(use-package smex)

(use-package projectile
  :defer t)

(use-package counsel
  :config 
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-s") 'swiper))


(use-package ivy-rich
  :ensure t
  :after (ivy)
  :init
  (setq ivy-rich-path-style 'abbrev
        ivy-virtual-abbreviate 'full)
  :config (ivy-rich-mode 1))


(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)
  (setq which-key-side-window-location 'top))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
        company-echo-delay 0
        company-dabbrev-downcase nil
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance))

  ;; Global keys
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous))

(use-package lsp-mode
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

;; Javascript
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :config 
  (add-hook 'js2-mode-hook #'lsp))


(use-package lsp-pwsh
  :load-path "~/.emacs.d/user-site-lisp"
  :hook (powershell-mode . (lambda () (require 'lsp-pwsh) (lsp)))
  :defer t)

(use-package emacs-lisp-mode
  :ensure nil
  :mode (("\\.el\\'" . emacs-lisp-mode))
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda ()
    (company-mode)
    (setq-local company-backends
                '(company-elisp
                  company-dabbrev
                  company-files
                  company-keywords
                  company-capf)))))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode 1))
  

;; Emacs Lisp
;; (add-hook 'emacs-lisp-mode-hook (lambda ()
;;                                   (company-mode)
;;                                   (setq-local company-backends
;;                                               '(company-elisp
;;                                                 company-dabbrev
;;                                                 company-files
;;                                                 company-keywords
;;                                                 company-capf))))

(use-package org-plus-contrib
  :mode (("\\.org\\'" . org-mode))
  :config
  (setq org-startup-indented t
	org-log-done 'time
	org-log-into-drawer t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((powershell . t)
     (dot . t)
     (emacs-lisp . t)))
  )

(use-package plantuml-mode)
(use-package graphviz-dot-mode)


(defun ubf|find-file ()
  (interactive)
  (find-file user-init-file))


(defun ubf|eshell-switch ()
  (interactive)
  (if(equal (buffer-name) "*eshell*")
      (switch-to-buffer (other-buffer (current-buffer) 1))
    (eshell)))


  (setq eshell-prompt-function
  	    (lambda ()
  	      (concat
  	       (propertize "┌─[" 'face `(:foreground "green"))
  	       (propertize (user-login-name) 'face `(:foreground "red"))
  	       (propertize "@" 'face `(:foreground "green"))
  	       (propertize (system-name) 'face `(:foreground "blue"))
  	       (propertize "]──[" 'face `(:foreground "green"))
  	       (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow"))
  	       (propertize "]──[" 'face `(:foreground "green"))
  	       (propertize (concat (eshell/pwd)) 'face `(:foreground "white"))
  	       (propertize "]\n" 'face `(:foreground "green"))
  	       (propertize "└─>" 'face `(:foreground "green"))
  	       (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "green"))
  	       )))

