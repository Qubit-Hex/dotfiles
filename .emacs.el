;; CONFIGURATION: V.0.1
;; DISPLAY SETTINGS

(global-display-line-numbers-mode)  ; display line numbers within current buffer
(menu-bar-mode 1) ; disable the menu 
(tool-bar-mode 0) ; Disable tool bar
(scroll-bar-mode 0) ;;  remove the scroll bar since neo tree gets fucked up....
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(setq sql-mysql-program "/opt/lampp/bin/mysql")
(set-face-attribute 'default nil :height 85)

;; FILE BEHAVIOUR 
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq inhibit-splash-screen t)  ; disable the default emacs splash screen
(setq-default tab-width 4)
(setq lsp-enable-snippet nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)  ; add the latex mod hook to the application
(set-default 'truncate-lines t)


;; PACKAGE LOCATIONS 
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                        ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; ORG BABEL CONFIGURATION FOR ORG FILES 
(org-babel-do-load-languages
 'org-babel-load-languages '(
			                 (python . t)
							 (latex . t)
							 (gnuplot . t) 
							 ))


;; ORG MODE CONFIGURATION
;; Latex preview options.

 '(org-format-latex-options
   (quote
    (:foreground default
				 :background whitex
				 :scale 3.0
				 :html-foreground "Black"
				 :html-background "Transparent"
				 :html-scale 3.0
				 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")
				 )
	)
   )


;; display inline images on start up org file
;; use this for math symbols within my org files.
(setq org-startup-with-inline-images t)


;; ============================================
;;          START CUSTOM COMMANDS
;; ===========================================

;; Open the Emacs config file 
(defun emacs-config-file()
  "Open the init file"
  (interactive)
  (find-file user-init-file))



;; Spawn a new shell in a buffer good for starting background servers, shells etc... 
(defun spawn-shell()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N)
  )


;; ENABLE REG EX SEARCHING
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	      (funcall (cdr my-pair))))
  )

;; auto insert a comment for labeling the file. for langs with /* comments 
(defun insert-jsdoc (filename description)
  "Insert a JS doc comment with file, and description at the top of the file."
  (interactive "sEnter file name: \nsEnter file description: ")
  (beginning-of-buffer)
  (insert (format "/*\n *\n *  @file: %s \n *\n *  @description: %s \n *\n */\n\n" filename description)))


;; Insert a JS Doc on the current line the cursor is at, good for automating function doc inserts..  /* comments 
(defun insert-comment (function-name description)
  "Inserts a block comment with the given function name and description on the current line in the specified format."
  (interactive "sEnter title information: \nsEnter description: ")
  (beginning-of-line)
  (insert (format "/*\n *\n *  %s \n *\n *  @description: %s \n *\n */\n" function-name  description)))



(defun insert-pydoc (filename description)
  "Insert a Python docstring with file, and description at the top of the file."
  (interactive "sEnter file name: \nsEnter file description: ")
  (beginning-of-buffer)
  (insert (format "\"\"\"\n    %s\n\n    %s\n\n\"\"\"\n\n" filename description)))


(defun insert-python-block-commnt (description funcName)
  "Insert a Python docstring with file, and description at the top of the file."
  (interactive "sEnter method / function  name: \nsEnter file description: ")
  (beginning-of-line)
  (insert (format "\"\"\"\n    %s\n\n    %s\n\n\"\"\"\n\n" funcName description)))



(defun popup-shell ()  
    "Create a new eshell buffer and display it at the bottom of the frame with a height of 10 lines."
  (interactive)
  (let ((height 20)
        (main-win (selected-window)))
    (split-window main-win (- height))
    (other-window 1)
    (eshell)))



;; ============================================
;;          END OF CUSTOM COMMANDS
;; ===========================================


;; ==========================================
;;           CUSTOM KEYBOARD SHORTCUTS
;; =========================================


(global-set-key (kbd "C-c e") 'neotree-toggle) ;; toggle neo tree
(global-set-key (kbd "C-c f") 'projectile-find-file) ; fuzzy browsing  
(global-set-key (kbd "C-c s") 'save-buffer) ;; save the current buffer
(global-set-key (kbd "C-c q") 'quit-window) ;; kill the current window and
(global-set-key (kbd "C-c a") 'org-agenda) ;; toggle the org agenda menu  for easier access
(global-set-key (kbd "C-c /") 'popup-shell) ;; toggle a popup shell for the current buffer


;; =============================================
;;          END OF CUSTOM KEYBOARD SHORTCUTS
;; =============================================



;; ==========================================================================
;;          Package START
;; =========================================================================

;; make sure emacs knows about the user defined path vars. 
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; enable popup for auto complete of commands
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

;; expand region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
("C--" . er/contract-region)))


;; web-mode CONFIGURATION FOR WEB FILES.
(use-package web-mode
  :ensure t
  :commands web-mode 
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode)
	 ("\\.css\\'" . web-mode)
	 ("\\.scss\\'" . web-mode)
     ("\\.php\\'" . web-mode)
     ("\\.blade\\'" . web-mode) 
     ))

;; company
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode))

;; company box
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; magit git intergration 
(use-package magit
  :ensure t
  :bind (
	 ("C-x g" . magit-status)))

;; C++ / C auto completion 
(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  )

;; Emacs Lsp Mode
(use-package lsp-mode
  :ensure t
  :commands lsp-deferred
  :hook (
	 (web-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration)
	 ))

;; python server LSP
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                          (lsp))))
;; Java LSP
(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp)
  :hook (java-mode . (lambda ()
                       (require 'lsp-java)
                       (lsp)))
  )

;; lsp haskell mode 
(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . (lambda ()
                          (require 'lsp-haskell)
                          (lsp)))
  )

;; haskell-mode
(use-package haskell-mode
  :ensure t 
  )

;; PHP MODE
(use-package php-mode
  :ensure t
  )

;; DOCKER INTERGRATION

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))


;; BULLET MODE FOR ORG MODE.
(use-package org-bullets
  :ensure t
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

;; projectile for searching projects
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  )

;; all the icons
(use-package all-the-icons
  :ensure t
  )

;; neo tree
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 48)
  )

;; helm 
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x c o" . helm-occur)
         ("M-y" . helm-show-kill-ring)
         ("C-x r b" . helm-filtered-bookmarks))
  :config (helm-mode 1))


;;

;; doom theme packages
(use-package doom-themes
  :ensure t
  )

