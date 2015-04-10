; package --- Summary
;;; Commentary:
;;; moving to elpa for package management, use the list that follows to
;;; add packages to be installed on load
;;; Code:

;;; Add /usr/local/bin to exec path
(setq exec-path (append exec-path '("/usr/local/bin")))

;;list of packages to be installed by elpa
(defvar installed-packages)

(setq installed-packages '(
			   color-theme
			   color-theme-solarized
			   yasnippet
			   js2-mode
			   js2-refactor
			   fullscreen-mode
			   auto-save-buffers-enhanced
			   magit
			   flycheck
			   dired+
			   nav
			   yaml-mode
			   html-to-markdown
			   markdown-mode+
			   tidy
			   slime
			   clojure-mode
			   cider
			   smartparens
			   rainbow-delimiters
			   cedet
			   helm
			   jedi
			   iedit
			   expand-region
			   python-mode
			   elpy
			   flake8
			   autopair
			   fill-column-indicator
			   undo-tree
			   desktop-registry
			   flymake-jslint
			   org
			   haskell-mode
			   projectile
			   flx-ido
			   company-mode
			   ggtags
			   perspective
			   persp-projectile
			   helm-projectile
			   zenburn-theme
			   monokai-theme
			   ))				


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

(global-set-key (kbd "C-<f11>") 'toggle-fullscreen)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
;			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://stable.melpa.org/packages/")))


;; init elpa
(setq package-enable-at-startup nil)
(package-initialize)

;; refresh contents if not present
(when (not package-archive-contents) (package-refresh-contents))

;;; install packages from the list if not installed already
(dolist (package installed-packages)
  (when (and (not (package-installed-p package))
	     (assoc package package-archive-contents))
    (message "package is %s" package)
    (package-install package)))

;; color and font
(load-theme 'solarized-dark t t)
(load-theme 'solarized-light t t)
(set-face-attribute 'default nil :height 140 :font "Monaco-14")
;;(enable-theme 'solarized-dark)
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-clarity)
;; (load-theme 'zenburn t)
(load-theme 'monokai t)


;; delete for terminal
(normal-erase-is-backspace-mode 1)

;;; auto complete
;;(require 'auto-complete-config)
;;(ac-config-default)

;;(defadvice ac-common-setup (after give-yasnippet-highest-priority activate)
;;  
;;  (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
;;  (add-to-list 'ac-sources 'ac-source-yasnippet))


;;; hide tool bar
(if window-system
    (tool-bar-mode -1)
)

;;; show line numbers
(global-linum-mode t)

;;; autosave
(require 'auto-save-buffers-enhanced)
  (auto-save-buffers-enhanced t)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; cider for clojure
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;;; smartparens
(require 'smartparens-config)

;;; rainbow delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)

;;; dired+
(require 'dired+)


;;; revert buffers when backing file changes
(global-auto-revert-mode t)

;;ls dired issue
(set-variable 'ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

;;; project explorer
(require 'nav)
(nav-disable-overeager-window-splitting)
;(require 'project-explorer)

;; (defun open-project-explorer ()
;;   "Custom project explorer configuration."
;;   (interactive)
;;   (set-variable 'pe/width 25)
;;   (set-variable 'pe/cache-enabled 't)
;;   (set-variable 'pe/directory-tree-function 'pe/get-directory-tree-simple)
;;   (set-variable 'pe/auto-refresh-cache 'f)
;;   (setq pe/omit-regex (concat pe/omit-regex "\\|\.pyc$"))
;;   (project-explorer-open))

(global-set-key (kbd "C-M-l") 'nav-toggle)


;;jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(require 'jedi)

;;;expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; python
(elpy-enable)

;;; autopair
(require 'autopair)
(autopair-global-mode)

;;; ess
;(require 'ess-site)

;;;fci
(add-hook 'python-mode-hook 'fci-mode)
(set-variable 'fci-rule-column 79)
(set-variable 'fci-rule-use-dashes t)
(set-variable 'fci-dash-pattern '0.2)
(set-variable 'fci-rule-color "darkblue")
(require 'fill-column-indicator)

;; ido
(require 'ido)
(ido-mode t)

;; cc
(require 'cc-mode)

;; flymake keys
(global-set-key [f2] 'flymake-display-err-menu-for-current-line)
(global-set-key [f3] 'flymake-goto-prev-error)
(global-set-key [f4] 'flymake-goto-next-error)


(custom-set-faces
 '(flymake-errline (( ((class color)) (:underline "Red" :background nil)) ))
 '(flymake-warnline ((((class color)) (:underline "Orange")))))


;;; magit
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

;;; undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo)

;;; cua
(cua-mode 1)

;;; hide menu bar for terminal,
;;; make shift up work on the termninal
;; (cond
;;  ((eq window-system 'x)
;;   (menu-bar-mode 1))
;;  (t
;;   (define-key input-decode-map "\e[1;2A" [S-up])
;;   (menu-bar-mode 0)))

;;; copy previous line - Ctrl-d
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(global-set-key (kbd "C-d") 'duplicate-line)

;;; desktops
;;  (desktop-save-mode 1)
;;  (desktop-registry-auto-register)

;;; close all buffers

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;; node 
;;; find node
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path
      '(
    "/usr/local/bin"
    "/usr/bin"
    "/bin"
    ))

;;;jshint
(add-to-list 'load-path "/Users/r/.nvm/v0.10.28/lib/node_modules/jshint-mode")
(require 'flymake-jshint)
(add-hook 'javascript-mode-hook
	  (lambda () (flymake-mode t)))

;;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; org-files
(setq org-todo-keywords
           '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-agenda-files '("~/Desktop/todo"))
(setq org-log-done 'time)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


(global-visual-line-mode t)

;;; haskell
(custom-set-variables
 '(haskell-mode-hook '(turn-on-haskell-indentation)))

;;; server
(server-start)

;;; projectile
(projectile-global-mode)
(setq projectile-switch-project-action 'helm-projectile)


;;; helm
(require 'helm-config)
(helm-mode 1)

;;; company mode
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "s-/") 'company-complete)

;;; recentf
(recentf-mode t)

;;; perspective
(persp-mode)
(require 'persp-projectile)

;;; helm projectile
(setq helm-projectile-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)
;(helm-autoresize-mode 1)


(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)

;;;(provide 'init)
;;; init ends here








