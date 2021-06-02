
# Table of Contents

1.  [General configuration](#org19ec18c)
    1.  [Global behaviours](#orgdf93c2a)
    2.  [Package configuration and initialisation](#orga00f10e)
2.  [EVIL!](#org1b8f169)
    1.  [Evil collection](#org4ef1e07)
    2.  [Evil surround](#org2864125)
3.  [Keybindings](#orgf338fd2)
    1.  [General](#org489fed7)
    2.  [Leader key emulation](#org8f53fe0)
    3.  [Misc](#orgf1bda63)
4.  [UI](#orgb53746a)
    1.  [Emacs configuration](#orgb6df909)
    2.  [Theme](#orgefdcbe3)
    3.  [All the icons](#orgb5b4ca6)
    4.  [Fringe](#orgcfda1a4)
    5.  [Line numbering](#orgd4244ce)
    6.  [Tabs](#orgcc33b85)
5.  [UX](#org7751f3b)
    1.  [Global](#org94c279f)
    2.  [Ivy and ivy-rich](#orgf20d67d)
    3.  [Helpful](#orgd949d59)
    4.  [Swiper](#orgf2b365a)
    5.  [Counsel](#org606f805)
    6.  [Which-key](#orgfb5dbe6)
    7.  [Neotree](#org8078493)
    8.  [Avy](#orgc7d9b53)
    9.  [Ace window](#org265da0a)
    10. [Expand region](#org513faf6)
    11. [Yes or no](#orgef86e05)
6.  [Org Mode](#org2e7401a)
    1.  [Configuration](#org91f242f)
    2.  [Org template](#org0666958)
    3.  [Org bullets](#org3068fca)
    4.  [Visual fill columns](#orgc94c627)
    5.  [Async execution](#org28f4391)
7.  [Development](#orge3d5941)
    1.  [Global](#orgcd37bfc)
    2.  [Indent guides](#orgb7edfd9)
    3.  [Projectile](#org8262f94)
    4.  [Language server protocol](#org7498ed8)
        1.  [lsp-mode](#org1d5213e)
        2.  [lsp-ivy](#org5ee3b80)
    5.  [Languages configuration](#org1474b47)
        1.  [Elisp](#org7974bbb)
        2.  [Javascript](#orgc190e77)
        3.  [Typescript](#org612373b)
        4.  [Haskell](#orga15e5fc)
        5.  [Purescript](#orga5534ce)
    6.  [Company](#org7f26b19)
    7.  [Flycheck](#org5b40e67)
    8.  [Magit](#org9898f61)

Catmacs is my own emacs configuration, inspired by System Crafter emacs from scratch series.


<a id="org19ec18c"></a>

# General configuration


<a id="orgdf93c2a"></a>

## Global behaviours

    
    (setq create-lockfiles nil)
    (setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
    (setq custom-file (concat user-emacs-directory "/custom.el"))


<a id="orga00f10e"></a>

## Package configuration and initialisation

    
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


<a id="org1b8f169"></a>

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
    
    (setq evil-shift-width 2)


<a id="org4ef1e07"></a>

## Evil collection

[Evil collection](https://github.com/emacs-evil/evil-collection) provide vim binding for the part of Emacs that are not covered by the base evil package.

    
    (use-package evil-collection
      :after evil
      :config
      (evil-collection-init))


<a id="org2864125"></a>

## Evil surround

[Evil surround](https://github.com/emacs-evil/evil-surround) is a port of vim surround.

    
    (use-package evil-surround
      :config
      (global-evil-surround-mode))


<a id="orgf338fd2"></a>

# Keybindings


<a id="org489fed7"></a>

## General

    
    (use-package general
      :config
      (general-create-definer catmacs/leader-key
        :keymaps '(normal visual emacs)
        :prefix "SPC"))


<a id="org8f53fe0"></a>

## Leader key emulation

    
    (catmacs/leader-key
      "ct" '(counsel-load-theme :which-key "choose theme")
      "cm" '(magit :which-key "magit")
      "x" '(counsel-M-x :which-key "M-x")
      "w" '(evil-window-map :which-key "window management")
      "p" '(projectile-command-map :which-key "projectile"))


<a id="orgf1bda63"></a>

## Misc

    (global-unset-key (kbd "C-s"))
    
    (general-define-key
     "C-s" #'save-buffer
     "C-f" #'swiper
     "C-p" #'counsel-projectile-find-file)
    
    (general-define-key
     :keymaps 'transient-base-map
     "<escape>" 'transient-quit-one)


<a id="orgb53746a"></a>

# UI


<a id="orgb6df909"></a>

## Emacs configuration

    
    (setq inhibit-startup-message t)
    (scroll-bar-mode -1)        
    (tool-bar-mode -1)         
    (tooltip-mode -1)         
    (menu-bar-mode -1)       
    (blink-cursor-mode 0)   
    (set-face-attribute 'default nil :font "Fira Code" :height 110 :weight 'semi-bold)


<a id="orgefdcbe3"></a>

## Theme

I use the excellent doom theme as a base for my custom palenight (mainly swapped colors around).

    
    (use-package doom-modeline
      :init (doom-modeline-mode 1))
    
    (use-package doom-themes
      :config
      (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
            doom-themes-enable-italic t) ; if nil, italics is universally disabled
      :init (load-theme 'catmacs-palenight t))


<a id="orgb5b4ca6"></a>

## All the icons

Add Icons. The first time you load your configuration on a new machine, you'll need to run the following command interactively so that mode line icons display correctly
`M-x all-the-icons-install-fonts`

    
    (use-package all-the-icons)


<a id="orgcfda1a4"></a>

## Fringe

Configure a fringe left and right of the buffer. We set the background to nil so it takes the same color has the background color of the buffer.

    
    (set-fringe-mode 15)
    (set-face-attribute 'fringe nil :background nil)


<a id="orgd4244ce"></a>

## Line numbering

I Use relative line numbering everywhere except for org-mode and some term-mode, where I don't display any numbering.

    
    (setq display-line-numbers-type 'relative)
    
    (global-display-line-numbers-mode t)
    
    (dolist (mode '(org-mode-hook
                    term-mode-hook
                    shell-mode-hook
                    neotree-mode-hook
                    eshell-mode-hook))
      (add-hook mode (lambda () (display-line-numbers-mode 0))))


<a id="orgcc33b85"></a>

## Tabs

[Centaur tabs](https://github.com/ema2159/centaur-tabs) is a highly configurable tab plugin

    
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


<a id="org7751f3b"></a>

# UX


<a id="org94c279f"></a>

## Global

Make escape quit everything

    
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit)


<a id="orgf20d67d"></a>

## Ivy and ivy-rich

    
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


<a id="orgd949d59"></a>

## Helpful

Helpful provide better description messages

    
    (use-package helpful
      :custom
      (counsel-describe-function-function #'helpful-callable)
      (counsel-describe-variable-function #'helpful-variable)
      :bind
      ([remap describe-function] . counsel-describe-function)
      ([remap describe-command] . helpful-command)
      ([remap describe-variable] . counsel-describe-variable)
      ([remap describe-key] . helpful-key))


<a id="orgf2b365a"></a>

## Swiper

    
    (use-package swiper)


<a id="org606f805"></a>

## Counsel

    (use-package counsel
      :bind (("M-x" . counsel-M-x)
             ("C-x b" . counsel-ibuffer)
             ("C-x C-f" . counsel-find-file)
             :map minibuffer-local-map
             ("C-r" . 'counsel-minibuffer-history)))


<a id="orgfb5dbe6"></a>

## Which-key

Provide shortcut and key description

    
    (use-package which-key
      :init (which-key-mode)
      :diminish which-key-mode
      :config
      (setq which-key-idle-delay 0.75))


<a id="org8078493"></a>

## Neotree

[Neotree](https://github.com/jaypei/emacs-neotree) is a tree plugin inspired by NerdTree for vim

    
    (use-package neotree
      :config
      (setq neo-theme 'icons))
    
    
    (catmacs/leader-key
      "n" '(neotree-toggle :which-key "neotree"))


<a id="orgc7d9b53"></a>

## Avy

[Avy](http://github.com/abo-abo/avy) is a easy motion inspired jump anywhere package.

    
    (use-package avy)
    
    (catmacs/leader-key
      "aa" '(avy-goto-line :which-key "avy line")
      "as" '(avy-goto-char :which-key "avy char")
      "ad" '(avy-goto-word-or-subword-1 :which-key "avy word"))


<a id="org265da0a"></a>

## TODO Ace window


<a id="org513faf6"></a>

## Expand region

    
    (use-package expand-region
      :bind ("C-=" . er/expand-region))


<a id="orgef86e05"></a>

## Yes or no

Respond at the "yes or no" question with just y or n

    
    (fset 'yes-or-no-p 'y-or-n-p)


<a id="org2e7401a"></a>

# Org Mode


<a id="org91f242f"></a>

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


<a id="org0666958"></a>

## Org template

Some shortcuts for the most used language, elisp, sh and javascript. Use it with `<el` + `TAB` to generate a source block.

    
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
    (require 'org-tempo)


<a id="org3068fca"></a>

## Org bullets

Make bullet prettier.

    
    (use-package org-bullets
      :after org
      :hook (org-mode . org-bullets-mode)
      :custom
      (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


<a id="orgc94c627"></a>

## Visual fill columns

Add some margin left and right of an org documents.

    (defun catmacs/org-mode-visual-fill ()
      (setq visual-fill-column-width 100
            visual-fill-column-center-text t)
      (visual-fill-column-mode 1))
    
    (use-package visual-fill-column
      :hook (org-mode . catmacs/org-mode-visual-fill))


<a id="org28f4391"></a>

## TODO Async execution

[ob-async](https://github.com/astahlman/ob-async)

    
    (use-package ob-async)

org-sh-stream

    
    ;(expand-file-name "ob-shstream.el" "~/emacs.d/lisp/")
    
    ;(require 'ob-shstream)


<a id="orge3d5941"></a>

# Development


<a id="orgcd37bfc"></a>

## Global

Define the default offset to two spaces

    
    (setq c-basic-offset 2)
    (setq-default
     indent-tabs-mode nil 
     tab-width 2)
    (setq indent-line-function 'insert-tab)


<a id="orgb7edfd9"></a>

## Indent guides

    
    (use-package highlight-indent-guides)
    (setq highlight-indent-guides-method 'character)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)


<a id="org8262f94"></a>

## Projectile

Add Project management utilies: go to file, go to project, switch between test and implementation, etc

    
    (use-package projectile
      :diminish projectile-mode
      :config (projectile-mode)
      :custom ((projectile-completion-system 'ivy))
      :init
      (setq projectile-project-search-path '("~/work/thecodeisgreen" "~/prog"))
      (setq projectile-switch-project-action #'projectile-dired))
    
    (use-package counsel-projectile
      :config (counsel-projectile-mode))


<a id="org7498ed8"></a>

## Language server protocol

IDE features are provided with languages servers and lsp-mode.


<a id="org1d5213e"></a>

### lsp-mode

lsp-mode connect to language server and give access to code completion, definition, references, refactoring and more.

    
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

lsp need some tuning for better performances. See this [page](https://emacs-lsp.github.io/lsp-mode/page/performance/)

    
    (setq gc-cons-threshold 100000000)
    (setq read-process-output-max (* 1024 3072))


<a id="org5ee3b80"></a>

### lsp-ivy

[lsp-ivy](https://github.com/emacs-lsp/lsp-ivy) Provide searching for symbol in project
Example of commands:

-   `lsp-ivy-workspace-symbol` - Search for a symbol name in the current project workspace
-   `lsp-ivy-global-workspace-symbol` - Search for a symbol name in all active project workspaces

    
    (use-package lsp-ivy)


<a id="org1474b47"></a>

## Languages configuration


<a id="org7974bbb"></a>

### Elisp

Add parens coloring when editing lisp code

    
    (use-package rainbow-delimiters
      :hook (emacs-lisp-mode . rainbow-delimiters-mode))


<a id="orgc190e77"></a>

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
          (call-process-shell-command (concat "eslint --fix " (buffer-file-name))))
        
        (eval-after-load 'rjsx-mode
          '(add-hook 'rjsx-mode-hook
                     (lambda ()
                       (add-hook 'after-save-hook #'catmacs/eslint-fix-file nil t))))

3.  TODO Flycheck

4.  TODO REPL

    look into [nodejs-repl](https://github.com/abicky/nodejs-repl.el) 


<a id="org612373b"></a>

### Typescript

1.  Configuration

        
        (use-package typescript-mode
          :mode "\\.ts\\'"
          :hook (typescript-mode . lsp-deferred)
          :config
          (setq typescript-indent-level 2))

2.  Eslint

    As with javascript, we run eslint automatically when saving file
    
        
        (eval-after-load 'typescript-mode
          '(add-hook 'typescript-mode-hook
                     (lambda ()
                       (add-hook 'after-save-hook #'catmacs/eslint-fix-file nil t))))


<a id="orga15e5fc"></a>

### Haskell

    
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


<a id="orga5534ce"></a>

### TODO Purescript


<a id="org7f26b19"></a>

## Company

    
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


<a id="org5b40e67"></a>

## Flycheck

[Flycheck](https://flycheck.org) is a linter that replace Flymake, the built in syntax checker.

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


<a id="org9898f61"></a>

## Magit

Provide a really nice git integration

    
    (use-package magit)
    (use-package forge)

