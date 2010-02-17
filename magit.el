(add-to-list 'load-path "~/work/dotemacs/magit")
(require 'magit)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c d") 'magit-display-process)