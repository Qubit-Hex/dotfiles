;; CONFIGURATION V.0.1
;;
;;

;;=====================================================================
;;        GLOBALS START 
;; ==================================================================

(set-default 'truncate-lines t)
(tool-bar-mode 0) ; Disable tool bar
(setq inhibit-splash-screen t)

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; =========================================================================
;;       GLOBAL END 
;; ========================================================================




;; ===============================================================================
;;        CUSTOM KEY BINDING START 
;; =============================================================================






;; ================================================================================
;;        CUSTOM KEY BINDING END 
;; ===============================================================================





;; ==========================================================================
;;          Package START 
;; =========================================================================


;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; Built-in project package
(require 'project)
(global-set-key (kbd "C-x p f") #'project-find-file)

(delete-selection-mode t)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(global-display-line-numbers-mode)

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


;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
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

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred



;; company
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)
(use-package company
  :ensure t
  :config (global-company-mode t))


(use-package company-box
  :hook (company-mode . company-box-mode))



;; eglot 
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; magit
(use-package magit
  :ensure t
  :bind (
	 ("C-x g" . magit-status)))


;; lsp-mode
(setq lsp-log-io nil) ;; Don't log everything = speed
(setq lsp-keymap-prefix "C-c l")
(setq lsp-restart 'auto-restart)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-code-actions t)

(use-package lsp-mode
  :ensure t
  :hook (
	 (web-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands lsp-deferred)


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package prettier-js
  :ensure t)
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
                              '("\\.tsx?\\'" . prettier-js-mode))))


(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    (treemacs-resize-icons 14)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)


;;=============== tag icons
(with-eval-after-load "treemacs"
  (setq treemacs-icon-tag-node-open-png   (propertize "− " 'face 'font-lock-keyword-face)
        treemacs-icon-tag-node-closed-png (propertize "+ " 'face 'font-lock-keyword-face)
        treemacs-icon-tag-leaf-png        (propertize "🞄 " 'face 'font-lock-keyword-face)))


;; =============================================================================
;;        END OF PACKAGES 
;; ============================================================================



(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-tokyo-night))
 '(custom-safe-themes
   '("90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "d824f0976625bb3bb38d3f6dd10b017bdb4612f27102545a188deef0d88b0cd9" "fa1b2c364b1d058d0611caa5f5c9b2e8cdd0eca519ef88af2de2a2728bbf8070" "803aaddee599b43da31fb0fd8fae0fa58b4ef7617c673f07201e3463a3099957" "2050674326d536ddd3dcea87e077d27071cfbbe974a4540b1a57b6b672f64c51" "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "eb122e1df607ee9364c2dfb118ae4715a49f1a9e070b9d2eb033f1cefd50a908" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "e87f48ec4aebdca07bb865b90088eb28ae4b286ee8473aadb39213d361d0c45f" "05626f77b0c8c197c7e4a31d9783c4ec6e351d9624aa28bc15e7f6d6a6ebd926" "3d2e532b010eeb2f5e09c79f0b3a277bfc268ca91a59cdda7ffd056b868a03bc" default))
 '(org-agenda-files '("~/Repos/Course-Work/agenda.org"))
 '(package-selected-packages
   '(treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil all-the-icons material-theme darktooth-theme challenger-deep-theme latex-math-preview lsp-tailwindcss gruvbox-theme flatland-theme monokai-theme centaur-tabs company-box company-php php-mode eglot doom-themes dracula-theme mode-icons auto-complete treemacs gruber-darker-theme prettier-js modus-operandi-theme magit company web-mode json-mode expand-region which-key exec-path-from-shell lsp-ui lsp-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
