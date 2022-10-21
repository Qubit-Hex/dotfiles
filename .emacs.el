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


;; i buffer configuration change it later to use-package. 
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

(defun spawn-shell()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(defun open-config-file()
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

;; REMAP C-x C-b to iBuffer since it works better
;; than the default buffer management

(global-set-key (kbd "C-x C-b") 'ibuffer)
; FIND a file with fzf 
(global-set-key (kbd "C-c p") 'counsel-fzf)
;; TERMINAL CONTROL FLOW 
(global-set-key  (kbd "C-c /") 'spawn-shell)
; toggle tree macs 
(global-set-key (kbd "C-c e") 'dired)
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


  
;; I also like this theme package, too so lets include this too... 
(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-dark t)
  )

(use-package doom-themes
  :ensure t
  )


;; =============================================================================
;;        END OF PACKAGES 
;; ============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-1337))
 '(custom-safe-themes
   '("7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
