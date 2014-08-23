(setq default-directory (file-name-directory load-file-name))
(setq package-user-dir
      (concat
       default-directory
       "elpa"))
(setq package-archives
      '(;; ("gnu" . "http://elpa.gnu.org/packages/")
	;; ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")
	))

(package-initialize)

(defun ensure-package-installed (packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(ensure-package-installed
 '(2048-game ace-window ace-jump-mode ample-theme auto-complete bookmark+ coffee-mode
	     color-theme es-windows icicles magit git-rebase-mode git-commit-mode
	     nodejs-repl popup projectile pkg-info epl dash s tabbar w3m yasnippet
	     dired+ go-mode go-autocomplete quickrun))

;;; set font
(add-to-list 'default-frame-alist '(font . "monaco-15"))
(global-linum-mode 1)
(show-paren-mode t)
(ample-theme)
(ac-config-default)
(icy-mode 1)
(iswitchb-mode 1)

(append '("/Users/airead/Downloads/mxcl-homebrew-2f541f3/bin/"
	  "/usr/local/bin/"
	  )
	exec-path)

(setq new-M-r (lookup-key global-map (kbd "C-x r")))
(global-set-key (kbd "M-r") new-M-r)

;;; easy keys to split window. Key based on ErgoEmacs keybinding
(global-set-key (kbd "M-3") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "M-4") 'split-window-vertically) ; split pane top/bottom
(global-set-key (kbd "M-1") 'split-window-horizontally) ; split pane left/right
(global-set-key (kbd "M-2") 'delete-window) ; close current pane
(global-set-key (kbd "M-8") 'other-window) ; cursor to other pane

(global-set-key (kbd "M-9") 'ace-window)

;;; tabbar
(tabbar-mode t)
(global-set-key [s-up] 'tabbar-backward-group)
(global-set-key [s-down] 'tabbar-forward-group)
(global-set-key (kbd "C-:") 'tabbar-backward)
(global-set-key (kbd "C-\"") 'tabbar-forward)

;;; yasnipets mode
(yas-global-mode 1)
(global-set-key (kbd "C-;") 'yas-expand)

;;; magit
(global-set-key "\C-ci" 'magit-status)

;;; recentf stuff
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;; highlight symbol 
(require 'highlight-symbol)
(global-set-key (kbd "C-'") 'highlight-symbol-at-point)
(global-set-key (kbd "C-M-'") 'highlight-symbol-remove-all)
(global-set-key (kbd "C-,") 'highlight-symbol-prev)
(global-set-key (kbd "C-.") 'highlight-symbol-next)

;;; nodejs-repl
(setq nodejs-repl-command "/usr/local/bin/node")

;;; use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; go code style
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

;;; quickrun
(global-set-key (kbd "<f9>") 'quickrun)

(quickrun-add-command "go"
                      '((:command . "go")
                        (:exec . ((lambda ()
                                    (if (string-match-p "_test\\.go\\'" (buffer-name))
                                        "%c test %o"
                                      "%c run %o %s %a"))))
                        (:compile-only . "%c build -o /dev/null %s %o %a")
                        (:description . "Compile go file and execute with 'go'")))
