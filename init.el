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
			   simp
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
(set-face-attribute 'default nil :height 135)

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
(elpy-use-ipython)

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


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

;;simp
(require 'simp)
(simp-project-define
 '(:has (.git)
	:ignore (.git *.pyc)))
(global-set-key (kbd "C-c f") 'simp-project-find-file)
(global-set-key (kbd "C-c d") 'simp-project-root-dired)
(global-set-key (kbd "C-c p") 'simp-project-rgrep)
(global-set-key (kbd "C-c b") 'simp-project-ibuffer-files-only)
(global-set-key (kbd "C-c B") 'simp-project-ibuffer)

;;;(provide 'init)
;;; init ends here
