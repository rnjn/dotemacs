(add-to-list 'load-path "~/work/dotemacs/pymacs")
(require 'pymacs)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(add-to-list 'pymacs-load-path "~/work/dotemacs/pymacs")
(pymacs-load "ropemacs" "rope-")


