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
  (let ((ensure nil)
	(first-confirm nil))
    (mapcar
     (lambda (package)
       (if (package-installed-p package)
	   nil
	 (unless first-confirm
	   (setq first-confirm t)
	   (if (y-or-n-p (concat (format "Package %s is missing.  Install it?" package)))
	     (setq ensure t)))
	 (if ensure
	     (package-install package)
	   package)))
    packages)))

;; make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(ensure-package-installed
 '(ace-window ace-jump-mode ample-theme auto-complete bookmark+ coffee-mode
	     color-theme es-windows icicles magit git-rebase-mode git-commit-mode
	     nodejs-repl popup projectile pkg-info epl dash s tabbar w3m yasnippet
	     dired+ go-mode go-autocomplete quickrun sourcemap flycheck git-gutter+
	     git-gutter-fringe+ highlight-indentation js3-mode paredit
	     js2-mode ac-js2 js2-refactor tern tern-auto-complete helm helm-projectile
             goto-last-change))

(add-to-list 'load-path (expand-file-name "lib/" config-base-dir))

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
(setq-default indent-tabs-mode nil)

;;;
(setq exec-path (append '("/Users/airead/Downloads/mxcl-homebrew-2f541f3/bin/"
	  "/usr/local/bin/"
	  )
	exec-path))

;;; easy keys to split window.  Key based on ErgoEmacs keybinding
(global-set-key (kbd "C-M-1") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "C-M-2") 'split-window-vertically) ; split pane top/bottom
(global-set-key (kbd "C-M-3") 'split-window-horizontally) ; split pane left/right
(global-set-key (kbd "C-M-4") 'delete-window) ; close current pane
(global-set-key (kbd "M-8") 'other-window) ; cursor to other pane

(global-set-key (kbd "M-9") 'ace-window)

;;; tabbar
(tabbar-mode t)
(global-set-key [s-up] 'tabbar-backward-group)
(global-set-key [s-down] 'tabbar-forward-group)
(global-set-key (kbd "C-:") 'tabbar-backward)
(global-set-key (kbd "C-\"") 'tabbar-forward)

;;; yasnipets mode
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" config-base-dir))
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
            (message "coffee hook")
	    (setq whitespace-action '(auto-cleanup))
	    (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
	    (whitespace-mode 1)
	    (highlight-indentation-mode 1)
	    (highlight-indentation-current-column-mode 1)
	    (coffee-cos-mode t)
	    (flycheck-mode t)
            (define-key coffee-mode-map (kbd "M-.") 'helm-etags-select)
            (auto-complete-mode 1)
	    ))

;;; html mode
(add-hook 'html-mode-hook
	  (lambda ()
            (auto-complete-mode 1)
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
            (setq whitespace-action '(auto-cleanup))
            (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
            (whitespace-mode 1)
            (highlight-indentation-mode 1)
            (highlight-indentation-current-column-mode 1)
            (ac-js2-mode)
            (tern-mode t)
            (tern-ac-setup)
            (flycheck-mode t)
            (set-variable 'indent-tabs-mode nil)
            (set (make-local-variable 'parens-require-spaces) nil)
            (setq tab-width 4)
            ))

;;; python mode
(add-hook 'python-mode-hook
          (lambda ()
            (setq whitespace-action '(auto-cleanup))
            (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
            (whitespace-mode 1)
            (highlight-indentation-mode 1)
            (highlight-indentation-current-column-mode 1)
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
(global-set-key [(control f5)] 'flycheck-list-errors)
(setq flycheck-check-syntax-automatically '(mode-enabled save new-line))

;;; goto-last-change
(global-set-key (kbd "C-`") 'goto-last-change)

;;; helm
(setq helm-command-prefix-key "C-c h")
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-M-o") 'helm-projectile)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(global-set-key (kbd "C-M-`") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)

;;; gtags
(custom-set-variables
 '(helm-gtags-prefix-key "C-t")
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))

(dolist (hook '(coffee-mode-hook
                emacs-lisp-mode-hook
                ))
  (add-hook hook (lambda () (helm-gtags-mode 1))))

;; key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     ;; (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack)))

;;; TODO hs mode
;; (require 'hideshowvis)

(dolist (hook '(coffee-mode-hook
                emacs-lisp-mode-hook
                ))
  (add-hook hook (lambda () (hs-minor-mode 1)
                    (message "lisp hook")
                 )))

(global-set-key (kbd "M-o h") 'hs-hide-all)
(global-set-key (kbd "M-o s") 'hs-show-all)
(global-set-key (kbd "M-o l") 'hs-hide-level)
(global-set-key (kbd "M-o M-o") 'hs-toggle-hiding)

(defvar new-M-r (lookup-key global-map (kbd "C-x r")))
(global-set-key (kbd "M-r") new-M-r)

(provide '.emacs)

;;; .emacs ends here
