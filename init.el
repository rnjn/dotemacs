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
			   auto-complete
			   js2-mode
			   ac-js2
			   js2-refactor
			   fullscreen-mode
			   auto-save-buffers-enhanced
			   magit
			   flycheck
			   dired+
			   project-explorer
			   yaml-mode
			   html-to-markdown
			   markdown-mode+
			   tidy
			   slime
			   ac-slime
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
			   ess
			   fill-column-indicator
			   undo-tree
			   projectile
			   desktop-registry
			   flymake-jslint
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
			 ("marmalade" . "http://marmalade-repo.org/packages/")))
;;			 ("melpa" . "http://melpa.milkbox.net/packages/")))


;; init elpa
(package-initialize)

;; refresh contents if not present
(when (not package-archive-contents) (package-refresh-contents))

(require 'assoc)

;;; install packages from the list if not installed already
(dolist (package installed-packages)
  (when (and (not (package-installed-p package))
	     (assoc package package-archive-contents))
    (message "package is %s" package)
    (package-install package)))

;; color and font
(load-theme 'solarized-dark t)
(set-face-attribute 'default nil :height 145)

;; delete for terminal
(normal-erase-is-backspace-mode 1)

;;; auto complete
(require 'auto-complete-config)
(ac-config-default)

(defadvice ac-common-setup (after give-yasnippet-highest-priority activate)
  (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
  (add-to-list 'ac-sources 'ac-source-yasnippet))

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

;;; helm
(require 'helm-config)

;;; revert buffers when backing file changes
(global-auto-revert-mode t)

;;; project explorer
(require 'project-explorer)

(defun open-project-explorer ()
  "Custom project explorer configuration."
  (interactive)
  (set-variable 'pe/width 25)
  (setq pe/omit-regex (concat pe/omit-regex "\\|\.pyc$"))
  (project-explorer-open))

(global-set-key (kbd "C-M-l") 'open-project-explorer)

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
(require 'ess-site)

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

;;; undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

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

;;; projectile
(add-hook 'elpy-mode-hook 'projectile-on)
(setq projectile-enable-caching t)

;;; desktops
(desktop-save-mode 1)
(desktop-registry-auto-register)

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
    ))

;;;jshint
(add-to-list 'load-path "/Users/r/.nvm/v0.10.28/lib/node_modules/jshint-mode")
(require 'flymake-jshint)
(add-hook 'javascript-mode-hook
	  (lambda () (flymake-mode t)))

;;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;(provide 'init)
;;; init ends here
