;; CONFIGURATION: V.0.1
;; AUTHOR+: QUBIT-HEX

;; DISPLAY SETTINGS
(set-frame-parameter (selected-frame) 'alpha '(95 . 95)) ; trasnparent setting
(add-to-list 'default-frame-alist '(alpha . (95 . 95))) ; transparent settings
(set-face-attribute 'default nil :height 87)
(set-default 'truncate-lines t)
(global-display-line-numbers-mode)  ; display line numbers within current buffer
(menu-bar-mode 1) ; disable the menu 
(tool-bar-mode 0) ; Disable tool bar
(scroll-bar-mode -1) ; remove the scroll bar since neo tree gets fucked up....


;; FILE BEHAVIOUR 
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq inhibit-splash-screen t)  ; disable the default emacs splash screen
(setq-default tab-width 4  indent-tabs-mode nil) ; default tabbing  behaviour.
(auto-save-visited-mode 1) 
(setq auto-save-timeout 3) ; auto save the file after 3 seconds have passed


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
                             (R . t)
                             )
 )

;; ============================================
;;          START CUSTOM COMMANDS
;; ===========================================


(defun emacs-config-file()
  "Open the init file"
  (interactive)
  (find-file user-init-file))

(defun spawn-shell()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N)
  )

(defun count-lines-in-repo()
  "In order to count the lines of code within the git repo"
  (interactive)
  (shell-command "git ls-files | xargs cloc")
  )

;; ENABLE REG EX SEARCHING
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	      (funcall (cdr my-pair))))
  )


;; ============================================
;;          END OF CUSTOM COMMANDS
;; ===========================================


;; ==========================================
;;           CUSTOM KEYBOARD SHORTCUTS
;; =========================================

(global-set-key (kbd "C-c p") 'helm-find) ;; fuzzy finding
(global-set-key (kbd "C-c e") 'neotree-toggle) ;; toggle neo tree
(global-set-key (kbd "C-c f") 'helm-find-files) ; fuzzy browsing  
(global-set-key (kbd "C-c s") 'save-buffer) ;; save the current buffer
(global-set-key (kbd "C-c q") 'quit-window) ;; kill the current window and
(global-set-key (kbd "C-c a") 'org-agenda) ;; toggle the org agenda menu  for easier access


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

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
("C--" . er/contract-region)))


;; web-mode CONFIGURATION FOR WEB FILES.
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode)
	 ("\\.css\\'" . web-mode)
	 ("\\.scss\\'" . web-mode)
     ("\\.php\\'" . web-mode)
     ("\\.blade\\'" . web-mode) 
     )
  :commands web-mode)


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

;; LSP MODE
(use-package lsp-mode
  :ensure t
  :hook (
	 (web-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands lsp-deferred)

;; python server
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
  (setq neo-window-width 45)
  )

;; helm
(use-package helm
  :ensure t
  :demand
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x c o" . helm-occur)) ;SC
  ("M-y" . helm-show-kill-ring) ;SC
  ("C-x r b" . helm-filtered-bookmarks) ;SC
  :preface (require 'helm-config)
  :config (helm-mode 1)
  )

;; doom themes 
(use-package doom-themes
  :ensure t
  )


;;REST CLIENT MODE
(use-package restclient
  :ensure t
  )
