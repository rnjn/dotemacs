dotemacs
========

my dot emacs setup. uses ELPA to install a bunch of packages if not already installed.

first list packages that need to be installed

``` elisp
(setq installed-packages '(
			   color-theme
			   color-theme-solarized
			   yasnippet
			   .......))
```

initialise list of package archives

``` elisp
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
```

 refresh contents if not present

``` elisp
(when (not package-archive-contents) (package-refresh-contents))
```

install packages from the list if not installed already

``` elisp
(dolist (package installed-packages)
  (when (and (not (package-installed-p package))
	     (assoc package package-archive-contents))
    (message "package is %s" package)
    (package-install package)))
```
