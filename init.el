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
;;;(add-to-list 'load-path "~/work/dotemacs/rinari");
;;;(require 'rinari)

;;; cedet 
(add-to-list 'load-path "~/work/dotemacs/cedet")
(load-file "~/work/dotemacs/cedet/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;;;ecb
(load-file "~/work/dotemacs/ecb_init.el")

;;;revert buffer
(load-file "~/work/dotemacs/revert-buffer-init.el")

;;; nxhtml
(load-file "~/work/dotemacs/nxhtml-init.el")

;;;clojure mode
(add-to-list 'load-path "~/work/dotemacs/clojure-mode")
(require 'clojure-mode)

;;;yasnippets
(add-to-list 'load-path "~/work/dotemacs/yasnippets/")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/work/dotemacs/yasnippets/snippets")

;;;magit
(load-file "~/work/dotemacs/magit.el")

;;; color-theme
(add-to-list 'load-path "~/work/dotemacs/color-theme")
(require 'color-theme)

;;; pymacs and rope
(add-to-list 'load-path "~/work/dotemacs/pymacs")
(load-file "~/work/dotemacs/pymacs-init.el")

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


