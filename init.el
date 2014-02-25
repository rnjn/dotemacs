;;; package --- Summary
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

;;; install packages from the list if not installed already
(dolist (package installed-packages)
  (when (and (not (package-installed-p package))
	     (assoc package package-archive-contents))
    (message "package is %s" package)
    (package-install package)))


;; color and font
(load-theme 'solarized-light t)
(set-face-attribute 'default nil :height 140)

;;; auto complete
(require 'auto-complete-config)
(ac-config-default)

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
(require 'project-explorer-autoloads)
(set-variable 'pe/width 25)

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

;;;(provide 'init)
;;; init ends here







