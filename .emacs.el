;; CONFIGURATION: V.0.1

;; window transparency setting
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
(set-default 'truncate-lines t)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(global-display-line-numbers-mode)  ; display line numbers within current buffer
(global-set-key (kbd "C-x C-b") 'buffer-menu)


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

;; ENABLE IVY MODE  
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)



;; ==========================================
;;           CUSTOM KEYBOARD SHORTCUTS
;; =========================================

;; DEFAULT KEY WILL BE <CTRL>c + <CUSTOM COMAND>

; find a file with fzf 
(global-set-key (kbd "C-c p") 'counsel-find-file)

;; TERMINAL CONTROL FLOW 
(global-set-key  (kbd "C-c /") 'multi-term)

; toggle tree macs 
(global-set-key (kbd "C-c e") 'treemacs)
; toggle the buffers
(global-set-key (kbd "C-c b") 'buffer-menu)

(global-set-key (kbd "C-c t") 'centaur-tabs--create-new-tab)
(global-set-key (kbd "C-c n") 'centaur-tabs-forward)
(global-set-key (kbd "C-c N") 'centaur-tabs-backward)


;; =============================================
;;          END OF CUSTOM KEYBOARD SHORTCUTS
;; =============================================




;; ==========================================================================
;;          Package START
;; =========================================================================

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-plain-icons t)
  (setq centaur-tabs-set-bar 'over)
  
 )

(use-package multi-term
  :ensure t
  :config
  (setq mult-term-program "/bin/bash")
  )

		 
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))

;; json-mode
(use-package json-mode
  :ensure t)

;; ALL THE ICONS
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  )

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
(use-package treemacs
  :ensure t
  :config
  (treemacs-resize-icons 14)
  )


;; ENABLE FUZZY SEARCHING OF FILES AND COMMANDS 
 (use-package counsel
    :ensure t
    :config
    (use-package flx
      :ensure t)
    (ivy-mode 1)
    (setq ivy-height 20)
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))


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
