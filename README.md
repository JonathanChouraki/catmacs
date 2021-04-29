
# Table of Contents

1.  [Kitchen sink](#org6c7808e)
2.  [EVIL!](#orga75a4a1)
    1.  [Evil collection](#orgeddc52c)
    2.  [Evil surround](#orgf6c79e3)
3.  [UI](#orga851f6f)
    1.  [Theme](#org168b9d3)
    2.  [Line numbering](#org926c618)
4.  [Keybindings](#orgef89ba6)
5.  [UX](#org114bcb5)
    1.  [Avy](#org158d95f)
    2.  [Ace window](#org6078e6c)
6.  [Org Mode](#org5d55820)
    1.  [Configuration](#org68ef57e)
    2.  [Org template](#orgfd00c08)
    3.  [Org bullets](#orga722e3f)
    4.  [Visual fill columns](#orgc362750)
7.  [Development](#org2302b82)
    1.  [Language server protocol](#orgaa4aa1d)
        1.  [lsp-mode](#org8f2de25)
        2.  [lsp-ivy](#org15a1fe0)
    2.  [Languages configuration](#orgd498636)
        1.  [Elisp](#orgec1d7fa)
        2.  [Javascript](#org9a8ea48)
        3.  [Typescript](#org648bdbd)
        4.  [Haskell](#org6214c76)
        5.  [Purescript](#org27645a8)
    3.  [Company](#org3803fb9)

Catmacs is my own emacs configuration, inspired by System Crafter emacs from scratch series.


<a id="org6c7808e"></a>

# Kitchen sink

    
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
    
    (set-face-attribute 'fringe nil :background nil) ; Set the background of the fringe the theme background color
    
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


<a id="orga75a4a1"></a>

# EVIL!

I use [Evil](https://github.com/emacs-evil/evil) for my vim needs. C-f and C-p are unbinded to use swiper and cousel-project-find-file instead.

    
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


<a id="orgeddc52c"></a>

## Evil collection

[Evil collection](https://github.com/emacs-evil/evil-collection) provide vim binding for the part of Emacs that are not covered by the base evil package.

    
    (use-package evil-collection
      :after evil
      :config
      (evil-collection-init))


<a id="orgf6c79e3"></a>

## Evil surround

[Evil surround](https://github.com/emacs-evil/evil-surround) is a port of vim surround.

    
    (use-package evil-surround
      :config
      (global-evil-surround-mode))


<a id="orga851f6f"></a>

# UI


<a id="org168b9d3"></a>

## Theme

I use the excellent doom theme as a base for my custom palenight (mainly swapped colors around).

    
    (use-package doom-modeline
      :init (doom-modeline-mode 1))
    
    (use-package doom-themes
      :config
      (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
            doom-themes-enable-italic t) ; if nil, italics is universally disabled
      :init (load-theme 'catmacs-palenight t))


<a id="org926c618"></a>

## Line numbering

I Use relative line numbering everywhere except for org-mode and some term-mode, where I don't display any numbering.

    
    (setq display-line-numbers-type 'relative)
    
    (global-display-line-numbers-mode t)
    
    (dolist (mode '(org-mode-hook
                    term-mode-hook
                    shell-mode-hook
                    eshell-mode-hook))
      (add-hook mode (lambda () (display-line-numbers-mode 0))))


<a id="orgef89ba6"></a>

# Keybindings

Custom keybinding, prefixed with general.

    
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
      "l" '(lsp-command-map :which-key "lsp")
      "p" '(projectile-command-map :which-key "projectile"))


<a id="org114bcb5"></a>

# UX


<a id="org158d95f"></a>

## Avy

[Avy](http://github.com/abo-abo/avy) is a easy motion inspired jump anywhere package.

    
    (use-package avy)
    
    (catmacs/leader-key
      "aa" '(avy-goto-line :which-key "avy line")
      "as" '(avy-goto-char :which-key "avy char")
      "ad" '(avy-goto-word-or-subword-1 :which-key "avy word"))


<a id="org6078e6c"></a>

## TODO Ace window


<a id="org5d55820"></a>

# Org Mode


<a id="org68ef57e"></a>

## Configuration

    
    (defun catmacs/org-mode-setup ()
      (org-indent-mode 1)
                                            ;(variable-pitch-mode 1)
      (visual-line-mode 1))
    
    (use-package org
      :hook (org-mode . catmacs/org-mode-setup)
      :config
      (setq org-ellipsis " ▾"))

Adding bash and javascript to the org-babel loaded languages.

    
    (require 'ob-js)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (js . t)))


<a id="orgfd00c08"></a>

## Org template

Some shortcuts for the most used language, elisp, sh and javascript. Use it with `<el` + `TAB` to generate a source block.

    
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
    (require 'org-tempo)


<a id="orga722e3f"></a>

## Org bullets

Make bullet prettier.

    
    (use-package org-bullets
      :after org
      :hook (org-mode . org-bullets-mode)
      :custom
      (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


<a id="orgc362750"></a>

## Visual fill columns

Add some margin left and right of an org documents.

    (defun catmacs/org-mode-visual-fill ()
      (setq visual-fill-column-width 100
            visual-fill-column-center-text t)
      (visual-fill-column-mode 1))
    
    (use-package visual-fill-column
      :hook (org-mode . catmacs/org-mode-visual-fill))


<a id="org2302b82"></a>

# Development


<a id="orgaa4aa1d"></a>

## Language server protocol

IDE features are provided with languages servers and lsp-mode.


<a id="org8f2de25"></a>

### lsp-mode

lsp-mode connect to language server and give access to code completion, definition, references, refactoring and more.

    
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


<a id="org15a1fe0"></a>

### lsp-ivy

[lsp-ivy](https://github.com/emacs-lsp/lsp-ivy) Provide searching for symbol in project
Example of commands:

-   `lsp-ivy-workspace-symbol` - Search for a symbol name in the current project workspace
-   `lsp-ivy-global-workspace-symbol` - Search for a symbol name in all active project workspaces

    
    (use-package lsp-ivy)


<a id="orgd498636"></a>

## Languages configuration


<a id="orgec1d7fa"></a>

### Elisp


<a id="org9a8ea48"></a>

### Javascript

1.  Configuration

    To use lsp with javascript, the `typescript-language-server` and `typescript` npm package must be available globally. To install it run:
    
        
        yarn global add typescript typescript-language-server
    
    FIXME: try to detect if a js file is jsx or not.
    if yes use rjsx
    else use js2-mode ?
    
        
        (use-package rjsx-mode
          :mode ("\\.jsx?$" . rjsx-mode)
          :config (setq js-indent-level 2))

2.  Eslint

    Run `eslint --fix` automatically when saving file.
    
        (defun catmacs/eslint-fix-file ()
          (interactive)
          (shell-command (concat "eslint --fix " (buffer-file-name))))
        
        (eval-after-load 'rjsx-mode
          '(add-hook 'rjsx-mode-hook
                     (lambda ()
                       (add-hook 'after-save-hook #'catmacs/eslint-fix-file nil t))))

3.  TODO Flycheck

4.  TODO REPL

    look into [nodejs-repl](https://github.com/abicky/nodejs-repl.el) 


<a id="org648bdbd"></a>

### TODO Typescript


<a id="org6214c76"></a>

### TODO Haskell


<a id="org27645a8"></a>

### TODO Purescript


<a id="org3803fb9"></a>

## Company

    
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

