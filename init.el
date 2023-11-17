;;; rnjns_dotemacs --- Summary
;;; Commentary:
;;; This is a simplistic Emacs setup.  For the last few years (2015-2021) I have been using
;;; Spacemacs and while it still works and I love the mnemonic keys and layered system
;;; it has become a slow, and I am tired of waiting for updates, breaking changes.  This is
;;; an attempt to declutter.  Updating the setup using what's now available in 2021.
;;; In a bind really whether to write literate configuration, choosing a simple single
;;; file for now, to avoid the clutter.
;;; Code:

;;; Packages
;;; setup where to pull packages from
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

;;; init use-package. Thanks to @irfn for pointing out use-package;
;;; it is a great addition. Lazy loading enables a faster setup and
;;; the added benefit of a clean config. If you are here, you may want to see
;;; https://github.com/irfn/emacs which is a far better emacs config

(package-install 'use-package)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;;; Add /usr/local/bin to exec path, load env vars
(use-package exec-path-from-shell
  :ensure t
  :config (setq exec-path-from-shell-variables '("PATH"))
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(use-package load-env-vars
  :ensure t
  :init (load-env-vars "~/.profile"))

(setq exec-path (append exec-path '("/usr/local/bin")))


;;list of packages to be installed by elpa

(use-package nerd-icons
  :ensure t)
;;; Fonts and Themes
(set-face-attribute 'default nil :font "Monaco-13")

(when (member "Fira Code" (font-family-list))
  (set-face-attribute 'default nil :font "Fira Code Retina-16"))

;;; too lazy to setup a list of safe themes
;;; this is dangerous, only controlled because I have a small
;;; controlled list of themes - solarized-light and material dark
(setq custom-safe-themes t)

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package material-theme
  :ensure t)

(load-theme 'material)


(use-package rainbow-delimiters
  :ensure t)

(use-package all-the-icons
;; Uncomment this while setting up a new machine.
;;  :config
;;  (all-the-icons-install-fonts)
  :ensure t)


;;; Bells and whistles
(setq ring-bell-function 'ignore )

(use-package diminish
  :defer 5
  :config  (diminish 'org-indent-mode)
  :ensure t)


;;; hints and shortcut discovery
(use-package which-key
  :diminish
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.1))

;;; Helm

(use-package helm
 :diminish
 :init
 (helm-mode t)
 :bind (("M-x"     . helm-M-x)
        ("C-x C-f" . helm-find-files)
        ("C-x b"   . helm-mini)     ;; See buffers & recent files; more useful.
        ("C-x r b" . helm-filtered-bookmarks)
        ("C-x C-r" . helm-recentf)  ;; Search for recently edited files
        ("C-c i"   . helm-imenu)
        ("C-h a"   . helm-apropos)
        ("M-y" . helm-show-kill-ring)

        :map helm-map
        ;; We can list ‘actions’ on the currently selected item by C-z.
        ("C-z" . helm-select-action)
        ("TAB"   . helm-execute-persistent-action)
        ("<tab>" . helm-execute-persistent-action))
 :config (progn
	   (setq helm-buffers-fuzzy-matching t)
	   (setq helm-recentf-fuzzy-matching t)
	   (setq helm-M-x-fuzzy-match t))
 :ensure t)

;; need for ripgrep speed
(use-package rg
  :config
  (global-set-key (kbd "M-s g") 'rg)
  (global-set-key (kbd "M-s d") 'rg-dwim)
  :ensure t)

(use-package helm-rg
  :ensure t)

;;; Dashboards and Projects

;;; to get a useful homescreen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "oh come on!")
  (setq dashboard-items '((recents . 5)
			  (projects . 5)
			  (agenda . 5)))
  )

(use-package all-the-icons
  :ensure t)
(use-package page-break-lines
  :ensure t)


(use-package projectile
  :ensure t
  :diminish
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Desktop/" "~/work/"))
  (setq projectile-switch-project-action 'helm-projectile)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)
	      ("C-c p" . projectile-command-map)))

(use-package helm-projectile
  :ensure t)

;;; Layout
(global-set-key (kbd "C-<f11>") 'toggle-fullscreen)
;; fullscreen post init
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(use-package nav
  :commands nav-toggle nav-disable-overeager-window-splitting
  :init
  (bind-key "C-M-l" 'nav-toggle)
  :ensure t)

(use-package ace-window
  :delight
  :bind ("M-o" . ace-window)
  :config (ace-window-display-mode 1)
  :ensure t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;;; hide tool bar
(if window-system
    (tool-bar-mode -1)
)
;;; show line numbers
(global-linum-mode 0)

(global-set-key (kbd "s-M-l") 'linum-mode)


;;; Finger memory
;;; cua
(cua-mode 1)


;;; File and folder management
(setq auto-save-default nil)
(use-package super-save
  :ensure t
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (super-save-mode +1)
  :diminish)

;;; Org Mode

(use-package org
  :ensure t
  :bind (("\C-ca" . org-agenda))
  :config (progn
            (setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "POSTPONED(p)" "|" "CANCELLED(c)"  "DONE(d)"))))
	    (setq org-agenda-files '("~/Desktop/diary"))
	    (setq org-log-done 'time)
            (add-hook 'org-shiftup-final-hook 'windmove-up)
            (add-hook 'org-shiftleft-final-hook 'windmove-left)
            (add-hook 'org-shiftdown-final-hook 'windmove-down)
            (add-hook 'org-shiftright-final-hook 'windmove-right)
            )
  :init (progn
	  (add-hook 'org-mode-hook 'flyspell-mode)
	  (add-hook 'org-mode-hook 'org-bullets-mode))
  :custom
  (org-archive-location "::* Completed")
  (org-startup-with-inline-images t)
  (org-modules '(org-crypt
                 org-habit
                 org-mouse
                 org-protocol
                 org-tempo))
  (org-tag-alist '(("@research" . ?s)
                   ("@reading" . ?r)
                   ("@writing" . ?w)
                   ("@spike" . ?k)
                   ("@home" . ?h)
                   ("@chore" . ?e)
                   ("@meeting" . ?m)
                   ("@notes" . ?n)
                   ("todo" . ?0)
                   ("@post" . ?p)))
  )


(use-package org-bullets
  :ensure t
  :config (setq org-bullets-bullet-list '("◉" "○" "⌗" "⌘" )))


;; Org journal
(use-package org-journal
  :ensure t
  :bind(("\C-cjs" . org-journal-search))
  :init
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-file-header "#+TITLE: %Y-%m-%d")
  (setq org-journal-dir "~/Desktop/diary")
  (setq org-journal-file-format "%Y-%m-%d.org")
  )

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.05)
                (org-level-3 . 1.0)
                (org-level-4 . 1.0)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil  :weight 'regular :height (cdr face) :box nil)
  )

;;; Programming

(use-package flycheck
  :ensure t
  :diminish
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :hook (
	 (prog-mode . flycheck-mode)))

(use-package company
  :ensure t
  :init (global-company-mode)
  :bind ("s-/" . company-complete)
  :config (
	   progn
	    (bind-key [remap-completion-at-point] #'company-complete company-mode-map)
	    (setq company-tooltip-align-annotations t)
	    (setq company-dabbrev-downcase t)
	    (setq company-idle-delay 0)
            (setq company-minimum-prefix-length 1))
  :diminish company-mode)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package yaml-mode
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (setq sp-show-pair-from-inside nil)
  (require 'smartparens-config)
  :diminish smartparens-mode)

(use-package yasnippet
  :ensure t
  :diminish
  :init
  (yas-global-mode 1))


(use-package magit
  :ensure t
  :bind (
	 ("C-c g" . magit-status))
  :config (setq magit-auto-revert-mode nil))

;; flymake keys and custom faces
(global-set-key [f2] 'flymake-display-err-menu-for-current-line)
(global-set-key [f3] 'flymake-goto-prev-error)
(global-set-key [f4] 'flymake-goto-next-error)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "Red" :background nil))))
 '(flymake-warnline ((((class color)) (:underline "Orange")))))



;;MISC
;;; revert buffers when backing file changes
(global-auto-revert-mode t)
;;; visual line mode
(global-visual-line-mode t)
;;; server
(server-start)
;;; show me the clock
(display-time-mode 1)
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " "))))
;;; recentf
(recentf-mode t)

;;; no need for scrollbars and window divider
(scroll-bar-mode -1)
(window-divider-mode -1)

;;; custom functions

(defun reload-dot-emacs ()
  "Reload init file."
  (interactive)
  (load-file "~/.emacs"))

(defun open-dot-emacs ()
  "Open this init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun toggle-fullscreen ()
  "Toggle full screen mode."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun duplicate-line()
  "Copy a line to the next line, mimics intellij."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))

(global-set-key (kbd "C-d") 'duplicate-line)

(defun close-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-themes nerd-icons yasnippet yaml-mode which-key use-package super-save solarized-theme smartparens rg rainbow-delimiters page-break-lines org-journal org-bullets nav material-theme magit load-env-vars helm-rg helm-projectile flycheck expand-region exec-path-from-shell doom-modeline diminish dashboard company ace-window)))
