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
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(setq use-package-always-ensure t)

(setq custom-file "~/.emacs.d/custom-settings.el")

(load custom-file t)

(desktop-save-mode t)

(setq user-full-name "Ulrik B. Farmen"
      user-mail-address "ulrik.bruun.farmen@gmail.com")

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p) 

;; Custom elisp
(add-to-list 'load-path "~/.emacs.d/user-site-lisp/")


;; Eyecandy
(load-theme 'material t)

;; (set-face-attribute 'default nil :font "Anonymous Pro" :height 120)
(set-face-attribute 'default nil :font "Consolas" :height 120)

;; (use-package all-the-icons)
  

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


;; (dolist (mode
;;          '(recentf-mode))
;;   (funcall mode 1))



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
  (general-evil-setup)
  (general-define-key :prefix "SPC" :states '(normal)
		      "," 'ubf|eshell-switch
		      "a" 'org-agenda
		      "bm" 'counsel-bookmark
		      "bb" 'switch-to-buffer
		      "c" 'evilnc-comment-or-uncomment-lines
		      "ff" 'counsel-find-file
		      "ef" 'edebug-defun
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

(use-package magit)

(use-package evil-magit)

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


;; (use-package lsp-pwsh
;;   :load-path "~/.emacs.d/user-site-lisp"
;;   :hook (powershell-mode . (lambda () (require 'lsp-pwsh) (lsp)))
;;   :defer t)

(use-package emacs-lisp-mode
  :ensure nil
  :mode (("\\.el\\'" . emacs-lisp-mode))
  :init
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


;; Listp Editing
(use-package smartparens
  :config
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package ace-window
  :config
  (ace-window-display-mode))
  

(use-package org-bullets
  :config
  (setq inhibit-compacting-font-caches t
	;; org-bullets-compose-leading-stars 'hide
	org-bullets-bullet-list
	'("■" "✿" "▲" "▶")))

(use-package org
  :mode (("\\.org\\'" . org-mode))

  :load-path ("~/.emacs.d/elpa/org-plus-contrib-20190402")

  :hook
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)

  :config
  (require 'ox-org)
  (require 'ox-html)

  (setq org-startup-indented t
	org-log-done 'time
	org-pretty-entities t
	org-blank-before-new-entry t
	org-hide-leading-stars t
	org-log-into-drawer t)

  (add-hook 'org-mode-hook (lambda ()
			     (company-mode)
			     (setq-local company-backends
					 '(company-dabbrev
					   company-files
					   company-keywords
					   company-yasnippet
					   company-capf))))

  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((powershell . t)
  ;;    (dot . t)
  ;;    (emacs-lisp . t)))

  ;; Orb publishing 

  (setq my-blog-header-file "~/repos/blog/org/partials/header.html"
        my-blog-footer-file "~/repos/blog/org/partials/footer.html"
        org-html-validation-link nil)

  ;; Load partials on memory
  (defun my-blog-header (arg)
    (with-temp-buffer
      (insert-file-contents my-blog-header-file)
      (buffer-string)))

  (defun my-blog-footer (arg)
    (with-temp-buffer
      (insert-file-contents my-blog-footer-file)
      (buffer-string)))


(defun org-publish-sitemap-customised (title list)
  "Default site map, as a string.
TITLE is the the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+SETUPFILE: ~/repos/blog/org/setup.setup" "\n\n" "#+TITLE: " title "\n\n" 
	  (org-list-to-org list)))

;; (defun org-html-add-navigation-after-content-div (string backend info)
;;   "Put the navigation menu inside the content div."
;;        (when (and (org-export-derived-backend-p backend 'html)
;;                   (string-match "div id=\"content\"" string))
;;          (replace-regexp-in-string "<div id=\"content\">" 
;; (format 
;; "
;; <div class=\"container\" id=\"bootstrap-container\">
;;   <div class=\"row\">
;;     <div class=\"col-sm-2\">
;;       One of three columns
;;     </div>
;;     <div class=\"content col-sm-6\">"
;; )string
;; )))

;; (add-to-list 'org-export-filter-final-output-functions
;;              'org-html-add-navigation-after-content-div)


(setq org-publish-project-alist
      '(
	("blog-notes"
         :base-directory "~/repos/blog/org"
         :base-extension "org"
         :publishing-directory "~/repos/blog/public"
         :recursive t
         :publishing-function ufarmen-html-publish-to-html
         :headline-levels 4
         :section-numbers nil
         ;; :html-head nil
         ;; :html-head-include-default-style nil
         :html-preamble my-blog-header
         :html-postamble my-blog-footer
	 :auto-sitemap t
	 :sitemap-function org-publish-sitemap-customised
         :sitemap-title "Home"
	 :sitemap-style list
         :sitemap-sort-folders ignore
         :sitemap-file-entry-format "%d - %t"

	 )

        ;; Define any other projects here...
        
          ;; For static files that should remain untouched
          ("blog-static"
           :base-directory "~/repos/blog/org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|eot\\|svg\\|woff\\|woff2\\|ttf"
           :publishing-directory "~/Projects/blog/public"
           :recursive t
           :publishing-function org-publish-attachment
           )

          ;; Combine the two previous components in a single one
          ("blog" :components ("blog-notes" "blog-static"))
	  ))   

      

;; (defun filter-local-links (link backend info)
;;   "Filter that converts all the /index.html links to /"
;;   (if (org-export-derived-backend-p backend 'html)
;;       (replace-regexp-in-string "/index.html" "/" link)))

;; Do not forget to add the function to the list!
;; (add-to-list 'org-export-filter-link-functions 'filter-local-links)

(defun org-ufarmen-sitemap-html-template (contents info)
  (message "inside custom!")
  (concat 
"<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">"

"<head>\n"
(org-html--build-meta-info info)
(org-html--build-head info)
(org-html--build-mathjax-config info)
"</head>\n"
"<body>\n"


"<div class=\"container\" id=\"bootstrap-container\">
  <div class=\"row\">
    <div class=\"col-sm-2\">
      This is from my custom template, mafakka!
    </div>
    <div id=\"main-content\" class=\"content col-sm-6\">"

"<h1 class=\"title\">Home</h1>"

contents

"</div>
    <div class=\"col-sm\">
      One of three columns madafakka
    </div>
  </div>
</div>"


(org-html--build-pre/postamble 'postamble info)
"</body>\n</html>"))



(defun org-ufarmen-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (if (string= "c:/Users/ufarmen/repos/blog/org/sitemap.org" (plist-get info :input-file))
      (org-ufarmen-sitemap-html-template contents info)
    (progn 
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	    (decl (or (and (stringp xml-declaration) xml-declaration)
		      (cdr (assoc (plist-get info :html-extension)
				  xml-declaration))
		      (cdr (assoc "html" xml-declaration))
		      "")))
       (when (not (or (not decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and org-html-coding-system
				  (fboundp 'coding-system-get)
				  (coding-system-get org-html-coding-system 'mime-charset))
			     "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (cond ((org-html-xhtml-p info)
		  (format
		   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
		   (plist-get info :language) (plist-get info :language)))
		 ((org-html-html5-p info)
		  (format " lang=\"%s\"" (plist-get info :language))))
	   ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
"
<div class=\"container\" id=\"bootstrap-container\">
  <div class=\"row\">
    <div class=\"col-sm-2\">
      One of three columns
    </div>
    <div id=\"main-content\" class=\"content col-sm-6\">"
   ;; Document title.
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
		       (plist-get info :title)))
	   (subtitle (plist-get info :subtitle))
	   (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	 (format
	  (if html5-fancy
	      "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
	    "<h1 class=\"title\">%s%s</h1>\n")
	  (org-export-data title info)
	  (if subtitle
	      (format
	       (if html5-fancy
		   "<p class=\"subtitle\">%s</p>\n"
		 (concat "\n" (org-html-close-tag "br" nil info) "\n"
			 "<span class=\"subtitle\">%s</span>\n"))
	       (org-export-data subtitle info))
	    "")))))
   contents

   "
</div>
    <div class=\"col-sm\">
      One of three columns madafakka
    </div>
  </div>
</div>
"
   ;; (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Closing document.
   "</body>\n</html>"))))


(org-export-define-derived-backend 'ufarmen-html 'html
  :translate-alist '((template . org-ufarmen-html-template))
  )

(defun ufarmen-html-publish-to-html (plist filename pub-dir)
(org-publish-org-to 'ufarmen-html filename ".html" plist pub-dir))

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

