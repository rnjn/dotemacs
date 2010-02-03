(defun reload-file-from-disk()
  "Refreshes current buffer without prompt"
  (interactive)
  (message "Refreshing file")
  (revert-buffer t t t))


(defun revert-all-buffers()
      "Refreshs all open buffers from their respective files"
      (interactive)
      (let* ((list (buffer-list))
	      (buffer (car list)))
        (while buffer
          (if (string-match "\\*" (buffer-name buffer)) 
	      (progn
	        (setq list (cdr list))
	        (setq buffer (car list)))
	      (progn
	        (set-buffer buffer)
	        (revert-buffer t t t)
	        (setq list (cdr list))
	        (setq buffer (car list))))))
      (message "Refreshing open files"))

(global-set-key (kbd "<f5>") 'reload-file-from-disk)
(global-set-key (kbd "C-<f5>") 'revert-all-buffers)
