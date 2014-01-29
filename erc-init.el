 (add-hook 'erc-after-connect
    	  '(lambda (SERVER NICK)
    	     (cond
    	      ((string-match "freenode\\.com" SERVER)
    	       (erc-message "PRIVMSG" "NickServ identify !abcd1234")))))

    

(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
          '(("freenode.net" "#thoughtworks")))


(require 'erc-match)
(setq erc-keywords '("rnjn"))
(erc-match-mode)

(require 'erc-track)
(erc-track-mode t) ; was (erc-track-modified-channels-mode t)
                   ; Note: erc-track-modified-channels-mode changed
                   ; to erc-track-mode as of erc-track.el
                   ; CVS revision 1.23 (November 2002)

(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)))

(require 'erc-fill)
(erc-fill-mode t)

(require 'erc-ring)
(erc-ring-mode t)

(require 'erc-netsplit)
(erc-netsplit-mode t)

(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

(erc-button-mode nil) ;slow

(setq erc-user-full-name "Ranjan Sakalley")
(setq erc-email-userid "gogo@thoughtworks.com")

;; logging:
(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory "~/.irclogs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)

(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
                                             (not (null buffer-file-name)))))))

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                       (set (make-variable-buffer-local
                                             'coding-system-for-write)
                                            'emacs-mule))))
;; end logging

;; Truncate buffers so they don't hog core.
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)


;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)


;;; Finally, connect to the networks.
(defun irc-connect ()
  (interactive)
  (erc :server "irc.freenode.com" :port 6667
                :nick "rnjn" :full-name "Ranjan Sakalley"))
