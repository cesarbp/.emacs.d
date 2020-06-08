(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'idle-highlight-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'smartparens-non-lisp)
(add-hook 'prog-mode-hook 'general-coding-hook)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;(add-hook 'prog-mode-hook 'highlight-parentheses-mode)


;;; Company

(setq company-minimum-prefix-length 2
      company-idle-delay 0.3)

;;; SmartParens
(defun smartparens-non-lisp ()
  (when (not (member major-mode '(emacs-lisp-mode)))
    (smartparens-mode)))

;;; Random stuff
(defun general-coding-hook ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (if window-system (hl-line-mode t))
  ;; Add easier to see foreground for dark themes
  ;; Makes it play better with idle-highlight too
  (set-face-attribute 'hl-line nil :distant-foreground "#00FFFF")
  (yas-minor-mode)
  (yas-reload-all))

;;; Avy
(global-set-key (kbd "C-:") 'avy-goto-char)

;;; Flymakers
(setq flymake-no-changes-timeout nil)
(setq flymake-start-syntax-check-on-newline nil)
(setq flycheck-check-syntax-automatically '(save mode-enabled))

;;; elisp slime nav
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; Web Mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))

(eval-after-load 'web-mode
  '(progn (setq web-mode-enable-current-element-highlight t)
          (setq web-mode-enable-current-column-highlight t)
          (setq web-mode-engines-alist
                '(("jinja"    . "\\.j2\\'")))))

;(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))

;;; Python
;; (eval-after-load 'elpy
;;   '(progn (elpy-enable)
;;           (setq python-shell-interpreter "ipython"
;;                 python-shell-interpreter-args "-i --simple-prompt")
;;           (add-to-list 'python-shell-completion-native-disabled-interpreters
;;                        "jupyter")
;;           ;(require 'py-autopep8)
;;           (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)))
;; (setq python-shell-interpreter "python"
;;       python-shell-interpreter-args "-i"
;;       python-shell-prompt-detect-failure-warning nil)
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
(when (string-equal system-type "darwin")
      (setq python-shell-interpreter "python3"))
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
(add-hook 'inferior-python-mode-hook
          (lambda () (setq-local scroll-conservatively 101)))

(defun add-company-python ()
  (when (boundp 'company-backends)
    (add-to-list 'company-backends '(company-anaconda :with company-capf))))
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'add-company-python)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)
(add-hook 'python-mode-hook 'yapf-mode)
(setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))


;; (defun projectile-pyenv-mode-set ()
;;   "Set pyenv version matching project name."
;;   (let ((project (projectile-project-name)))
;;     (if (member project (pyenv-mode-versions))
;;         (pyenv-mode-set project)
;;       (pyenv-mode-unset))))

;; (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

;;; javascript

(eval-after-load 'js2-mode
  '(progn (require 'js2-refactor)
          (require 'xref-js2)       ; requires the silversearcher (ag)
                                        ;(require 'indium)
          (js2r-add-keybindings-with-prefix "C-c C-r")
          (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
          ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
          ;; unbind it.
          (define-key js-mode-map (kbd "M-.") nil)
          ))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(add-hook 'js2-mode-hook #'js2-refactor-mode)

;;; haskell
(defconst haskell-mode-dir (concat emacs-pkg-dir "/haskell-mode"))
(defconst haskell-mode-autoloads-file
  (concat haskell-mode-dir "/haskell-mode-autoloads.el"))
(if (file-exists-p haskell-mode-autoloads-file)
    (load haskell-mode-autoloads-file)
  (warn "haskell-mode-autoloads.el needs to be generated manually, see github README of haskell-mode. Need to do 'make haskell-mode-autoloads.el' inside of haskell-mode dir. If it fails try rm haskell-mode-autoloads.el first if it exists"))

(add-to-list 'Info-default-directory-list haskell-mode-dir)
(add-hook 'haskell-mode-hook 'dante-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)
;; put dante-company at the end of the company-backends list
(defun dante-company-backends ()
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (setq company-backends (delete 'dante-company company-backends))
    (add-to-list 'company-backends 'company-dabbrev-code)
    ))
(add-hook 'dante-mode-hook 'dante-company-backends)
(add-hook 'dante-mode-hook
   '(lambda () (flycheck-add-next-checker 'haskell-dante
                '(warning . haskell-hlint))))

;;; SQL
(add-hook 'sql-mode-hook
          (lambda ()
            (make-local-variable 'indent-tabs-mode)
            (make-local-variable 'c-basic-offset)
            (setq indent-tabs-mode nil
                  c-basic-offset 4)))

;;; Wolfram
(autoload 'wolfram-mode "wolfram-mode" nil t)
(autoload 'run-wolfram "wolfram-mode" nil t)
(setq wolfram-program "/Applications/Wolfram Engine.app/Contents/Resources/Wolfram Player.app/Contents/MacOS/MathKernel")
(add-to-list 'auto-mode-alist '("\\.wl$" . wolfram-mode))

;;; Rust and LSP
(require 'rust-mode)
(setq rust-format-on-save t)
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
(add-hook 'rust-mode-hook #'lsp)
(setq lsp-keymap-prefix "s-l"
      lsp-rust-server 'rust-analyzer)
