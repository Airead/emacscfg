;;; .emacs --- airead Fan emacs config

;;; Code:

;;; Commentary:


(setq default-directory (concat (getenv "HOME") "/"))
(defvar config-base-dir (file-name-directory load-file-name))
(setq package-user-dir
      (concat
       config-base-dir
       "elpa"))
(custom-set-variables '(package-archives
			'(;; ("gnu" . "http://elpa.gnu.org/packages/")
			  ;; ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
			  ("melpa" . "http://melpa.milkbox.net/packages/")
			  )))

(package-initialize)

(defun ensure-package-installed (packages)
  "Assure every PACKAGES is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (concat (format "Package %s is missing.  Install it?" package)))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(ensure-package-installed
 '(ace-window ace-jump-mode ample-theme auto-complete bookmark+ coffee-mode
	     color-theme es-windows icicles magit git-rebase-mode git-commit-mode
	     nodejs-repl popup projectile pkg-info epl dash s tabbar w3m yasnippet
	     dired+ go-mode go-autocomplete quickrun sourcemap flycheck git-gutter+
	     git-gutter-fringe+ highlight-indentation js3-mode paredit
	     js2-mode ac-js2 js2-refactor tern tern-auto-complete helm))

(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;;; set font
(add-to-list 'default-frame-alist '(font . "monaco-15"))
(global-linum-mode 1)
(show-paren-mode t)
(ample-theme)
(ac-config-default)
(icy-mode 1)
(iswitchb-mode 1)

(setq exec-path (append '("/Users/airead/Downloads/mxcl-homebrew-2f541f3/bin/"
	  "/usr/local/bin/"
	  )
	exec-path))

;;; easy keys to split window.  Key based on ErgoEmacs keybinding
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
(global-set-key (kbd "C-'") 'highlight-symbol-at-point)
(global-set-key (kbd "C-M-'") 'highlight-symbol-remove-all)
(global-set-key (kbd "C-,") 'highlight-symbol-prev)
(global-set-key (kbd "C-.") 'highlight-symbol-next)

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


;;; coffee
(custom-set-variables '(coffee-tab-width 2)
		      '(coffee-args-compile '("-c" "-m")))
(add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)
(add-hook 'coffee-mode-hook
	  (lambda ()
	    (setq whitespace-action '(auto-cleanup))
	    (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
	    (whitespace-mode 1)
	    (highlight-indentation-mode 1)
	    (highlight-indentation-current-column-mode 1)
	    ))

;;; html
(add-hook 'html-mode-hook
	  (lambda ()
	    (set (make-local-variable 'sgml-basic-offset) 4)))

;;; projectile
(projectile-global-mode 1)

;;; move temp and autosave file to temporary dir
(setq create-lockfiles nil)
(setq make-backup-files nil)

;;; git gutter
(require 'git-gutter-fringe+)
(global-git-gutter+-mode 1)
;; Optional: Activate minimal skin
;; (git-gutter-fr+-minimal)

;;; javascript mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-highlight-level 3)
(add-hook 'js2-mode-hook
	  (lambda ()
	    ;; (setq whitespace-action '(auto-cleanup))
	    ;; (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
	    ;; (whitespace-mode 1)
	    ;; (highlight-indentation-mode 1)
	    ;; (highlight-indentation-current-column-mode 1)
	    ;; (auto-complete-mode 1)
	    (define-key js2-mode-map "{" 'paredit-open-curly)
	    (define-key js2-mode-map "}" 'paredit-close-curly-and-newline)
	    (my-paredit-nonlisp)
	    (ac-js2-mode)
	    (tern-mode t)
	    (tern-ac-setup)
	    ))


;; 在mac下显示中文字体
(defun show ()
  "Show chinese font."
  (interactive)
  (set-fontset-font
   (frame-parameter nil 'font)
   'han
   (font-spec :family "Hiragino Sans GB" ))
)
(add-hook 'emacs-startup-hook 'show)

;;; auto-complete
(global-auto-complete-mode 1)

;;; flycheck mode
(global-flycheck-mode 1)

(defvar new-M-r (lookup-key global-map (kbd "C-x r")))
(global-set-key (kbd "M-r") new-M-r)


(provide '.emacs)

;;; .emacs ends here
