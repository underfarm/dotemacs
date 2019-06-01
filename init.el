;; Actually, we can win back half of that 0.2s right away with a simple trick:
;; Less GC during startup


(setq package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      visible-bell t
      initial-scratch-message ";; ready\n\n"
      custom-file "~/.emacs.d/custom-settings.el"
      package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/"))
      ;; inhibit-startup-screen t
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
  (require 'use-package))

(setq use-package-always-ensure t)


(setq user-full-name "Ulrik B. Farmen"
      user-mail-address "ulrik.bruun.farmen@gmail.com")

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p) 

;; Custom elisp
(add-to-list 'load-path "~/.emacs.d/user-site-lisp/")
(add-to-list 'load-path "~/.emacs.d/partials/")


;; Look my own partials
(load custom-file t)

(mapc (lambda (name)
	(require (intern (file-name-sans-extension name))))
      (directory-files "~/.emacs.d/partials/" nil "\\.elc$"))

;; (mapc (lambda (name)
;;         (byte-compile-file name))
;;       (directory-files "~/.emacs.d/partials/" t "\\.el$"))

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text
           global-auto-revert-mode      ; Reverts the buffer if file has changed
           scroll-bar-mode              ; No scroll bars either
           blink-cursor-mode))          ; The blinking cursor gets old
  (funcall mode 0))


(dolist (mode
	 '(desktop-save-mode))
  (funcall mode 1))

;; Backups
;;; Disk space is cheap. Save lots.
(setq delete-old-versions -1
      version-control t
      vc-make-backup-files t
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; We need to add git to path, if not then magit will complain.
(setq ubf|additional-exec-paths '(
				  "c:/Program Files/git/bin"
				  "C:/Program Files/Git/usr/bin"
				  "C:/Users/ufarmen/scoop/shims"
			     ))

(setenv "PATH" (concat (getenv "PATH") (mapconcat 'identity ubf|additional-exec-paths ";")))
(setq exec-path (append exec-path ubf|additional-exec-paths))


;; Powershell
(use-package powershell
  :mode ("\\.ps1\\'" . powershell-mode)
  :config
  (require 'ps-ivy)
  (require 'company-powershell)
  (add-hook 'powershell-mode-hook (lambda ()
			     (company-mode)
			     (setq-local company-backends
					 '(company-dabbrev
					   company-files
					   company-pscommand-backend
					   company-keywords
					   company-yasnippet
					   company-capf))))
  ;; (require 'ob-powershell)
  ;; (add-hook 'powershell-mode-hook #'lsp)
  )


(use-package smex)

(use-package ag)


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
  (define-key company-active-map (kbd "C-k") #'company-select-previous)

  (add-hook 'emacs-lisp-mode-hook (lambda ()
				    (company-mode)
				    (prettify-symbols-mode)
				    (setq-local company-backends
						'(company-elisp
						  company-dabbrev
						  company-yasnippet
						  company-files
						  company-keywords
						  company-capf)))))


  )

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

(use-package projectile
  :after (counsel)
  :defer t
  :config
  (setq projectile-completion-system 'ivy))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)
  (setq which-key-side-window-location 'top))


(use-package lsp-mode
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

;; Javascript
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :config 
  (add-hook 'js2-mode-hook #'lsp))


;; (use-package lsp-pwsh
;;   :load-path "~/.emacs.d/user-site-lisp"
;;   :hook (powershell-mode . (lambda () (require 'lsp-pwsh) (lsp)))
;;   :defer t)
(use-package aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode))

(use-package magit)

(use-package evil-magit)

;; Listp Editing

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package ace-window
  :config
  (ace-window-display-mode))
  
(use-package plantuml-mode)
(use-package graphviz-dot-mode)
