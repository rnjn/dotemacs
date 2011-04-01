;;;general
(setq inhibit-startup-message t)
(line-number-mode t)
(setq visible-bell t)
(global-font-lock-mode t)
(setq exec-path (append exec-path '("/opt/local/bin")) )
(global-visual-line-mode 1)

;;;css mode
(load-file "~/work/dotemacs/css-mode.el")

;;;view config
(load-file "~/work/dotemacs/view.el")

;;;custom functions
(load-file "~/work/dotemacs/custom.el")

;;; interactive mode
(require 'ido)
 (ido-mode t)

;;; rails support
(add-to-list 'load-path "~/work/dotemacs/rinari/")
(require 'rinari)

;;; cedet 
(add-to-list 'load-path "~/work/dotemacs/cedet")
(require 'cedet) 
;;(require 'semantic)
;;(load-file "~/work/dotemacs/cedet/common/cedet.el")
;;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;;(global-srecode-minor-mode 1)   
(global-ede-mode 1)                      ; Enable the Project management system


;;;revert buffer
(load-file "~/work/dotemacs/revert-buffer-init.el")

;;; nxhtml
(load-file "~/work/dotemacs/nxhtml-init.el")

;;;clojure mode
(add-to-list 'load-path "~/work/dotemacs/clojure-mode")
(require 'clojure-mode)

;;;yasnippets
(add-to-list 'load-path "~/work/dotemacs/yasnippet/")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/work/dotemacs/yasnippet/snippets")

;;;magit
(load-file "~/work/dotemacs/magit.el")
;on mac
(add-to-list 'exec-path "/usr/local/git/bin")
(setq magit-git-executable "/usr/local/git/bin/git")

;;; pymacs and rope
;(add-to-list 'load-path "~/work/dotemacs/pymacs")
;(load-file "~/work/dotemacs/pymacs-init.el")

;;; autocomplete
(add-to-list 'load-path "~/work/dotemacs/auto-complete")
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-dwim t)

;;;(load-file "~/work/dotemacs/auto-python.el")

;;; flymake
(load-file "~/work/dotemacs/flymake-init.el")

;;; make emacs use the clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;; show line numbers
(load-file "~/work/dotemacs/linum.el")
(require 'linum)
(linum-mode t)

;;;js2 mode
(add-to-list 'load-path "~/work/dotemacs/js2")
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; transparency
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(99 50))
(add-to-list 'default-frame-alist '(alpha 99 50))

;;; color-theme
(add-to-list 'load-path "~/work/dotemacs/color-theme")
(require 'color-theme)
(color-theme-initialize)
;(load-file "~/work/dotemacs/themes/zen-and-art.el")
(color-theme-clarity)

;;;muse
(add-to-list 'load-path "~/work/dotemacs/muse/lisp")
(load-file "~/work/dotemacs/muse/lisp/muse.el")
(require 'muse-mode) 

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/work/dotemacs/elpa/package.el"))
  (package-initialize))

;;;scheme
(setq scheme-program-name "/Applications/mit-scheme.app/Contents/Resources/mit-scheme")

;;;f# mode
(add-to-list 'load-path "~/work/dotemacs/fsharp-mode")
(load-file "~/work/dotemacs/fsharp-mode/fsharp.el")

;;;backup folder
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups
