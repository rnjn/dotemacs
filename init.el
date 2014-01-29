;;; package --- Summary
;;; Commentary:
;;; moving to elpa for package management, use the list that follows to
;;; add packages to be installed on load
;;; Code:

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

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; init elpa
(package-initialize)

;; refresh contents if not present
(when (not package-archive-contents) (package-refresh-contents))

;;; install packages from the list if not installed already
(dolist (package installed-packages)
  (when (and (not (package-installed-p package))
	     (assoc package package-archive-contents))
    (package-install package)))

;; color and font
(load-theme 'solarized-dark t)
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

;;;(provide 'init)
;;; init ends here


