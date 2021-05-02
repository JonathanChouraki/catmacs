; Set backup out of the way
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq custom-file (concat user-emacs-directory "/custom.el"))
                                        ; == Packages == ;
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

                                        ; == UI == ;
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar
(blink-cursor-mode 0)       ; Not blinking cursor
(set-face-attribute 'default nil :font "Fira Code" :height 110 :weight 'semi-bold)

(use-package general
  :config
  (general-create-definer catmacs/leader-key
    :keymaps '(normal visual emacs)
    :prefix "SPC"))

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

                                        ; == UX == ;
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package swiper)

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

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.75))


                                        ; == Programming == ;
(setq-default
 indent-tabs-mode nil ; Use space for indenting
 tab-width 2)
                                        ;show-trailing-whitespace t) ; FIXME only show whitespace in prog mode
                                        ;(add-hook 'prog-mode-hook 'whitepace-mode)
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

(use-package magit)

(use-package forge)

(use-package org)

(global-unset-key (kbd "C-s")) ; unbind C-s to use save buffer instead

(general-define-key
 "C-s" #'save-buffer
 "C-f" #'swiper
 "C-p" #'counsel-projectile-find-file)

                                        ; escape quit transient window
(general-define-key
 :keymaps 'transient-base-map
 "<escape>" 'transient-quit-one)

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

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode))

(catmacs/leader-key
  "ct" '(counsel-load-theme :which-key "choose theme")
  "cm" '(magit :which-key "magit")
  "x" '(counsel-M-x :which-key "M-x")
  "w" '(evil-window-map :which-key "window management")
  "l" '(lsp-command-map :which-key "lsp")
  "p" '(projectile-command-map :which-key "projectile"))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :init (load-theme 'catmacs-palenight t))

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

(defun catmacs/lsp-mode-setup ()
  (lsp-enable-which-key-intgration)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :config 
  (lsp-enable-which-key-integration t)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :hook ((lsp-mode  catmacs/lsp-mode-setup)
         (rjsx-mode . lsp-deferred)))

(use-package lsp-ivy)

(use-package rjsx-mode
  :mode ("\\.jsx?$" . rjsx-mode)
  :config (setq js-indent-level 2))

(defun catmacs/eslint-fix-file ()
  (interactive)
  (shell-command (concat "eslint --fix " (buffer-file-name))))

(eval-after-load 'rjsx-mode
  '(add-hook 'rjsx-mode-hook
             (lambda ()
               (add-hook 'after-save-hook #'catmacs/eslint-fix-file nil t))))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
 (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))
