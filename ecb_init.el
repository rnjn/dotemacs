(add-to-list 'load-path "~/work/dotemacs/ecb")

(global-set-key (kbd "<f1>") 'ecb-activate)
(global-set-key (kbd "C-<f1>") 'ecb-deactivate)

(setq ecb-tip-of-the-day nil)
(setq ecb-auto-compatibility-check nil)

(global-ede-mode 1)
(semantic-load-enable-code-helpers)
(global-srecode-minor-mode 1)
 
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-activation-selects-ecb-frame-if-already-active t)
 '(ecb-compilation-buffer-names (quote (("*vc*") ("*vc-diff*") ("*anything*") ("*Apropos*") ("*eshell*") ("*Occur*") ("*shell*" . t) ("\\*[cC]ompilation.*\\*" . t) ("\\*i?grep.*\\*" . t) ("*Help*") ("*Completions*") ("*Backtrace*") ("*Compile-log*") ("*Messages*") ("*magit: " . t))))
 '(ecb-compile-window-height 6)
 '(ecb-compile-window-temporally-enlarge (quote both))
 '(ecb-compile-window-width (quote frame))
 '(ecb-layout-name "left7")
 '(ecb-maximize-ecb-window-after-selection t)
 '(ecb-mode-line-display-window-number nil)
 '(ecb-options-version "2.40")
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-indent 2)
 '(ecb-vc-enable-support t)
 '(ecb-windows-width 0.2))

(require 'ecb)
 
(provide 'ecb-init)