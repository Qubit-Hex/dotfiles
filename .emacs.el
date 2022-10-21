;; CONFIGURATION: V.0.1
;; AUTHOR+: QUBIT-HEX
;; PURPOSE: TO MAKE A C/C++ AND WEB DEVELOPMENT IDE FOR MY NEED THAT I DON'T HATE.


;; window transparency setting
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
(set-default 'truncate-lines t)
(set-face-attribute 'default nil :height 88) ; adjust the front size of the application 
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(global-display-line-numbers-mode)  ; display line numbers within current buffer
(menu-bar-mode 1) ; SHOW THE MENU BAR.
(tool-bar-mode 0) ; Disable tool bar
(scroll-bar-mode -1) ; remove the scroll bar since neo tree gets fucked up.....
(setq inhibit-splash-screen t)  ; disable the default emacs splash screen
(setq-default tab-width 4  indent-tabs-mode nil) ; default tabbing  behaviour.


(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                        ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; ENABLE CODE EXECUTATION IN ORG MODE WITH THE FOLLOWING LANAGUAGES.
(org-babel-do-load-languages
 'org-babel-load-languages '(
			     (python . t)
			     (sql . t)
			     (R . t)
			     ))

;; I buffer configuration  
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("emacs_config" (name . "^.emacs$"))
               ("org" (name . "^.*org$"))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("mu4e" (name . "\*mu4e\*"))
               ("programming" (or (mode . python-mode) (mode . c++-mode)))
               ("emacs" (or (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$"))))
              )
             )
      )



(add-hook 'ibuffer-mode-hook
(lambda ()
(ibuffer-auto-mode 1)
(ibuffer-switch-to-saved-filter-groups "default"))
)

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)

;; ============================================
;;          START CUSTOM COMMANDS
;; ===========================================


(defun spawn-shell()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N)
  )

(defun open-config()
  "Open the init file"
  (interactive)
  (find-file user-init-file)
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
(global-set-key (kbd "C-c f") 'helm-find-files) ; fuzzy browsing 
(global-set-key  (kbd "C-c /") 'spawn-shell) ;; spawn-shell
(global-set-key (kbd "C-c e") 'neotree-toggle) ;; neo tree toggle 
(global-set-key (kbd "C-c b") 'ibuffer) ;; current buffers that are in use.
(global-set-key (kbd "C-c s") 'save-buffer) ;; save the current buffer
(global-set-key (kbd "C-c q") 'quit-window) ;; kill the current window and

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
         ("\\.php\\'" . web-mode))
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

;; magit
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

;; neo tree
(use-package neotree
  :ensure t
  )

;; I also like this theme package, too so lets include this too... 
(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-dark t)
  )

;; all the themes from doom
(use-package doom-themes
  :ensure t
  )


;; =============================================================================
;;        END OF PACKAGES 
;; ============================================================================
