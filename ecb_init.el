(add-to-list 'load-path "~/work/dotemacs/ecb")
(require 'ecb)
(global-set-key (kbd "<f1>") 'ecb-activate)
(global-set-key (kbd "C-<f1>") 'ecb-deactivate)

(setq ecb-tip-of-the-day nil)
(setq ecb-auto-compatibility-check nil)
(setq ecb-layout-name "left2")