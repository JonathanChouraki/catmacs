(setq create-lockfiles nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq custom-file (concat user-emacs-directory "/custom.el"))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package evil
  :bind (("C-k" . evil-scroll-up)
         ("C-j" . evil-scroll-down))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (define-key evil-motion-state-map (kbd "C-f") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil))

(setq evil-shift-width 2)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode))

(use-package general
  :config
  (general-create-definer catmacs/leader-key
    :keymaps '(normal visual emacs)
    :prefix "SPC"))

(catmacs/leader-key
  "ct" '(counsel-load-theme :which-key "choose theme")
  "cm" '(magit :which-key "magit")
  "x" '(counsel-M-x :which-key "M-x")
  "w" '(evil-window-map :which-key "window management")
  "p" '(projectile-command-map :which-key "projectile"))

(global-unset-key (kbd "C-s"))

(general-define-key
 "C-s" #'save-buffer
 "C-f" #'swiper
 "C-p" #'counsel-projectile-find-file)

(general-define-key
 :keymaps 'transient-base-map
 "<escape>" 'transient-quit-one)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        
(tool-bar-mode -1)         
(tooltip-mode -1)         
(menu-bar-mode -1)       
(blink-cursor-mode 0)   
(set-face-attribute 'default nil :font "Fira Code" :height 110 :weight 'semi-bold)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :init (load-theme 'catmacs-palenight t))

(use-package all-the-icons)

(set-fringe-mode 15)
(set-face-attribute 'fringe nil :background nil)

(setq display-line-numbers-type 'relative)

(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                neotree-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-show-new-tab-button nil
   centaur-tabs-style "box"
   centaur-tabs-set-icons t
   centaur-tabs-gray-out-icons 'buffer
   centaur-tabs-set-close-button nil
   centaur-tabs-height 64
   centaur-tabs-set-modified-marker t
   centaur-tabs-cycle-scope 'tabs
   centaur-tabs-set-bar 'over)
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  :bind
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward)))

(catmacs/leader-key
  "tt" '(centaur-tabs--create-new-tab :which-key "new tab")
  "ts" '(centaur-tabs-counsel-switch-group :which-key "switch tabs group"))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
              ("TAB" . ivy-alt-done)
              ("C-l" . ivy-alt-done)
              ("C-j" . ivy-next-line)
              ("C-f" . ivy-previous-line)
              :map ivy-switch-buffer-map
              ("C-f" . ivy-previous-line)
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
              :map ivy-reverse-i-search-map
              ("C-k" . ivy-previous-line)
              ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package swiper)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.75))

(use-package neotree
  :config
  (setq neo-theme 'icons))


(catmacs/leader-key
  "n" '(neotree-toggle :which-key "neotree"))

(use-package avy)

(catmacs/leader-key
  "aa" '(avy-goto-line :which-key "avy line")
  "as" '(avy-goto-char :which-key "avy char")
  "ad" '(avy-goto-word-or-subword-1 :which-key "avy word"))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(fset 'yes-or-no-p 'y-or-n-p)

(defun catmacs/org-mode-setup ()
  (org-indent-mode 1)
                                        ;(variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . catmacs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

(require 'ob-js)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (js . t)))

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("js" . "src javascript"))
(require 'org-tempo)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun catmacs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . catmacs/org-mode-visual-fill))

(use-package ob-async)

;(expand-file-name "ob-shstream.el" "~/emacs.d/lisp/")

;(require 'ob-shstream)

(setq c-basic-offset 2)
(setq-default
 indent-tabs-mode nil 
 tab-width 2)
(setq indent-line-function 'insert-tab)

(use-package highlight-indent-guides)
(setq highlight-indent-guides-method 'character)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (setq projectile-project-search-path '("~/work/thecodeisgreen" "~/prog"))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(defun catmacs/lsp-mode-setup ()
  (lsp-enable-which-key-intgration)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-l")
  :config 
  (lsp-enable-which-key-integration t)
  (define-key lsp-mode-map (kbd "C-l") lsp-command-map)
  (setq lsp-log-io nil)
  :hook ((lsp-mode  catmacs/lsp-mode-setup)
         (rjsx-mode . lsp-deferred)))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 3072))

(use-package lsp-ivy)

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package rjsx-mode
  :mode ("\\.jsx?$" . rjsx-mode)
  :config (setq js-indent-level 2))

(defun catmacs/eslint-fix-file ()
  (interactive)
  (call-process-shell-command (concat "eslint --fix " (buffer-file-name))))

(eval-after-load 'rjsx-mode
  '(add-hook 'rjsx-mode-hook
             (lambda ()
               (add-hook 'after-save-hook #'catmacs/eslint-fix-file nil t))))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(eval-after-load 'typescript-mode
  '(add-hook 'typescript-mode-hook
             (lambda ()
               (add-hook 'after-save-hook #'catmacs/eslint-fix-file nil t))))

(use-package haskell-mode
  :config (setq haskell-indent-level 2))
(use-package lsp-haskell
  :after lsp-mode
  :config (setq lsp-haskell-formatting-provider "stylish-haskell"))
(use-package hindent)
(use-package flymake-hlint)
(setq haskell-process-type 'cabal-new-repl)
                                        ;(setq haskell-process-log t) 

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-common-or-cycle)
              ("RET" . company-complete))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.3))

(use-package company-box
  :hook (company-mode . company-box-mode))

(defun catmacs/use-local-eslint ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;(use-package flycheck
;  :hook (('after-init-hook #'global-flycheck-mode)
         ;(flycheck-mode catmacs/use-local-eslint))
  ;:config ((setq-default flycheck-disabled-checkers
                         ;(append flycheck-disabled-checkers
                                 ;'(javascript-jshint)))
           ;((flycheck-add-mode 'javascript-eslint 'rjsx-mode))))

(use-package magit)
(use-package forge)
