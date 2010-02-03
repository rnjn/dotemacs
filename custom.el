(defun reload-dot-emacs()
 (interactive)
 (load-file "~/.emacs"))
 
(defun open-dot-emacs()
 (interactive)
 (find-file "~/work/dotemacs/init.el"))

(defun open-shell-config()
 (interactive)
 (find-file "~/.zshrc"))
