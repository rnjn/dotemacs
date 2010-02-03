;;;general
(setq inhibit-startup-message t)
(line-number-mode t)
(setq visible-bell t)
(global-font-lock-mode t)

;;;view config
(load-file "~/work/dotemacs/view.el")

;;;custom functions
(load-file "~/work/dotemacs/custom.el")

;;; interactive mode
(require 'ido)
 (ido-mode t)

;;; rails support
(add-to-list 'load-path "~/work/dotemacs/rinari")
(require 'rinari)

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
(add-to-list 'load-path "~/work/dotemacs/magit")
(require 'magit)

;;; color-theme
(add-to-list 'load-path "~/work/dotemacs/color-theme")
(require 'color-theme)

;;; pymacs and rope
(load-file "~/work/dotemacs/pymacs-init.el")

;;; autocomplete
(add-to-list 'load-path "~/work/dotemacs/auto-complete")
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-dwim t)

;;; flymake
(load-file "~/work/dotemacs/flymake-init.el")

;;; make emacs use the clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;; show line numbers
(load-file "~/work/dotemacs/linum.el")
(require 'linum)
(linum-mode t)
