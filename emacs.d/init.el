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
(set-fringe-mode 15)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(blink-cursor-mode 0)       ; Not blinking cursor
(set-face-attribute 'default nil :font "Fira Code" :height 110 :weight 'semi-bold)

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :init (load-theme 'catmacs-palenight t))

(set-face-attribute 'fringe nil :background nil) ; Set the background of the fringe the theme background color

                                        ; == Evil == ;

(use-package evil
  :bind (("C-k" . evil-scroll-up)
         ("C-j" . evil-scroll-down))
  :init
  (setq evil-want-keybinding nil)
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-mode 1))

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

                                        ; == UX == ;
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq display-line-numbers-type 'relative)

(global-display-line-numbers-mode t)

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
              ("C-k" . ivy-previous-line)
              :map ivy-switch-buffer-map
              ("C-k" . ivy-previous-line)
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
              :map ivy-reverse-i-search-map
              ("C-k" . ivy-previous-line)
              ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

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

(use-package rjsx-mode
                                        ;FIXME: try to detect if a js file is jsx or not.
                                        ; if yes use rjsx
                                        ; else use js2-mode ?
  :config (setq js-indent-level 2)
  :mode ("\\.jsx?$" . rjsx-mode))

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

                                        ; == Keybindings == ;
(catmacs/leader-key
  "ct" '(counsel-load-theme :which-key "choose theme")
  "x" '(counsel-M-x :which-key "M-x")
  "w" '(evil-window-map :which-key "window management")
  "m" '(magit :which-key "magit")
  "p" '(projectile-command-map :which-key "projectile"))

(define-key evil-motion-state-map (kbd "C-f") nil) ; unbind C-f to use swiper instead
(define-key evil-normal-state-map (kbd "C-p") nil) ; unbind C-p to use counsel-project-find-file instead
(global-unset-key (kbd "C-s")) ; unbind C-s to use save buffer instead

(general-define-key
 "C-s" #'save-buffer
 "C-f" #'swiper
 "C-p" #'counsel-projectile-find-file)

                                        ; escape quit transient window
(general-define-key
 :keymaps 'transient-base-map
 "<escape>" 'transient-quit-one)
