
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

  ;; Make it pretty
  ;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/

(custom-theme-set-faces
 'user
 '(org-block                 ((t (:inherit fixed-pitch))))
 '(org-document-info         ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-link                  ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value        ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent                ((t (:inherit (org-hide fixed-pitch))))))

  (font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(let* ((variable-tuple
        (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  (require 'ox-org)
  (require 'ox-html)

  (setq org-startup-indented t
	org-log-done 'time
	org-hide-emphasis-markers t
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
      <nav id=\"navigation-right\">
        <ul>
<li> [ Home ] </li>
<li> [ Archive ] </li>
<li> [ About ] </li>
</ul>
      </nav>
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

(provide 'my-org)
