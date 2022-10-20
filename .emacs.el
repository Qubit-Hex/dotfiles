;; CONFIGURATION: V.0.1
;;


;; window transparency setting
(set-frame-parameter (selected-frame) 'alpha '(98 . 98))
(add-to-list 'default-frame-alist '(alpha . (98 . 98)))
(set-default 'truncate-lines t)
(set-face-attribute 'default nil :height 91) ; adjust the front size of the application 
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

;; ENABLE PACKAGE REPOS FOR THE PACKAGE MANAGER
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


;; I BUFFER CONFIGURATION 
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")
(setq ibuffer-saved-filter-groups
(quote (("default"
("dired" (mode . dired-mode))
("org" (name . "^.*org$"))
("web" (or (mode . web-mode) (mode . js2-mode)))
("shell" (or (mode . eshell-mode) (mode . shell-mode)))
("mu4e" (name . "\*mu4e\*"))
("programming" (or (mode . python-mode) (mode . c++-mode)))
("emacs" (or
(name . "^\\*scratch\\*$")
(name . "^\\*Messages\\*$")))
))))

(add-hook 'ibuffer-mode-hook
(lambda ()
(ibuffer-auto-mode 1)
(ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)


;; ============================================
;;          START CUSTOM COMMANDS
;; ===========================================

;; SPAWN A NEW SHELL INSTANCE EVERYTIME IT IS CALLED.
(defun spawn-shell()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

;; ============================================
;;          END OF CUSTOM COMMANDS
;; ===========================================

;; ==========================================
;;           CUSTOM KEYBOARD SHORTCUTS
;; =========================================

;; REMAP C-x C-b to iBuffer since it works better
;; than the default buffer management
;;

(global-set-key (kbd "C-x C-b") 'ibuffer)
; FIND a file with fzf 
(global-set-key (kbd "C-c p") 'counsel-fzf)
;; TERMINAL CONTROL FLOW 
(global-set-key  (kbd "C-c /") 'spawn-shell)
; toggle tree macs 
(global-set-key (kbd "C-c e") 'neotree-toggle)
; toggle the buffers
(global-set-key (kbd "C-c b") 'ibuffer)
;; save the current file
(global-set-key (kbd "C-c s") 'save-buffer)
;; kill the current buffer
(global-set-key (kbd "C-c q") 'kill-current-buffer)


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
  :config (global-company-mode t))

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

;; PYTHON LSP SERVER 
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; LSP UI.
(use-package lsp-ui
  :ensure t
  )


;; BULLET MODE FOR ORG MODE.
(use-package org-bullets
  :ensure t
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))


;; DEFAULT THEMES PACKAGE THAT I LIKE TO USE BY DEFAULT
(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-dark t)
 )

;; TREE MACS FILE EXPLORER. 
(use-package neotree
  :ensure t
  )


;; ENABLE FUZZY commands  
 (use-package counsel
    :ensure t
    :config
    (use-package flx
      :ensure t)
    (ivy-mode 1)
    (setq ivy-height 20)
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t) 
    )


;; TYPESCRIPT PACKAGE
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; =============================================================================
;;        END OF PACKAGES 
;; ============================================================================


;; ENABLE REG EX SEARCHING
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))
