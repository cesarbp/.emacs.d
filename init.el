;;;; ---- package management ----

;; be sure not to load stale bytecode-compiled lisp
(setq load-prefer-newer t)

;; this is where all subtree packages are
(defconst emacs-pkg-dir (concat user-emacs-directory "pkg"))

;; load up f, and its dependencies s and dash, so we can use `f-glob'
;; and `f-join'
(dolist (pkg '("f.el" "dash.el" "s.el"))
  (add-to-list 'load-path (concat emacs-pkg-dir "/" pkg)))
(require 'f) (require 's) (require 'dash)

;; helper function
(defun expand-all-globs (root globs)
  (let ((do-glob (lambda (glob) (f-glob (f-join root glob)))))
    (apply 'nconc (mapcar do-glob globs))))

;; now add all my pkg lisp directories
(let* ((globs '("*" "*/lisp"))
       (dirs (expand-all-globs emacs-pkg-dir globs)))
  (dolist (dir dirs)
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;; finally put my own site-lisp at the front of `load-path'
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

;; we will use use-package to load everything else
;; (require 'use-package)

;; (defun pnh-reinit-libs ()
;;   (interactive)
;;   (let ((generated-autoload-file (concat user-emacs-directory "my-autoload.el")))
;;     (dolist (d (directory-files (concat user-emacs-directory "pkg") t "^[^\.]"))
;;       (if (file-directory-p d)
;; 	  (progn
;; 	    (dolist (f (directory-files d t "\\.el$"))
;;               (byte-compile-file f))
;; 	    (update-directory-autoloads d))))))

;; (dolist (l (directory-files (concat user-emacs-directory "pkg") nil "^[^\.]"))
;;   (add-to-list 'load-path (concat user-emacs-directory "pkg/" l))
;;   (autoload (intern l) (concat l ".el")))

;; (when (not (file-exists-p (concat user-emacs-directory "my-autoload.el")))
;;   (pnh-reinit-libs))

;; (load (concat user-emacs-directory "my-autoload.el"))

;;; --- Packages --- ;;;
(require 'eldoc)
(require 'better-defaults)
(require 'smex)
;; ido
(require 'ido-completing-read+)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)
; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; magit
; dash already loaded previously
(require 'async)
(require 'magit-popup)
(require 'ghub)
(require 'with-editor)
(require 'magit)
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
	       (concat emacs-pkg-dir "/magit/Documentation/")))
;; Paredit
(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;; idle-highlight-mode  https://github.com/nonsequitur/idle-highlight-mode
(require 'idle-highlight-mode)
(defun idle-hl-mode-hook ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (if window-system (hl-line-mode t))
  (idle-highlight-mode t))
(add-hook 'emacs-lisp-mode-hook 'idle-hl-mode-hook)
;; End idle-highlight-mode
(require 'find-file-in-project)  ; https://github.com/technomancy/find-file-in-project
;; elisp slime nav
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)
;; window number
(require 'window-number)
(window-number-mode)
(window-number-meta-mode)
;; projectile
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; dashboard
(require 'page-break-lines)
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents  . 15)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
;; Color theme
(require 'color-theme)
(add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/atom-one-dark-theme/"))
(load-theme 'atom-one-dark t)
;; org mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
;; nix mode
(require 'nix-mode)
;; highlight current line number
(require 'hlinum)
(hlinum-activate)
(global-linum-mode)
(set-face-foreground 'linum "dim gray")
;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; Web mode
(require 'web-mode)
;(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-engines-alist
      '(("jinja"    . "\\.j2\\'")))
;; PHP mode
(require 'php-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282C34" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "DAMA" :family "Source Code Pro")))))

;; smart mode line
(require 'rich-minority)
(require 'smart-mode-line)
(sml/setup)
;; python
(require 'elpy)
(elpy-enable)
;; (setq python-shell-interpreter "python"
;;       python-shell-interpreter-args "-i"
;;       python-shell-prompt-detect-failure-warning nil)
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; javascript
;;(require 'indium)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(require 'js2-refactor)
(require 'xref-js2)  ; requires the silversearcher (ag)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
;; buffer-move
(require 'buffer-move)
(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)
;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; Haskell
(require 'epl)
(require 'pkg-info)
(require 'flycheck)
(require 'lcr)
(require 'haskell-mode)
(require 'dante)
(defconst haskell-mode-dir (concat emacs-pkg-dir "/" "haskell-mode"))
(add-to-list 'load-path haskell-mode-dir)
(add-to-list 'Info-default-directory-list haskell-mode-dir)
(add-hook 'haskell-mode-hook 'dante-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)
;; put dante-company at the end of the company-backends list
(defun remove-dante-company-from-head ()
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (setq company-backends (delete 'dante-company company-backends))
    (add-to-list 'company-backends 'company-dabbrev)))
(add-hook 'dante-mode-hook 'remove-dante-company-from-head)

(condition-case nil
    (require 'haskell-mode-autoloads)
  (error (message "haskell mode files need to be generated, see github README of haskell-mode. Need to do 'make haskell-mode-autoloads.el'")))
(setq flymake-no-changes-timeout nil)
(setq flymake-start-syntax-check-on-newline nil)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
;; custom
(setq inhibit-spash-screen t)
(setq inhibit-startup-message t)
(setq tab-always-indent 'complete)
(winner-mode 1)
(put 'erase-buffer 'disabled nil)
(setq shell-command-switch "-ic")
;; exec-path-from-shell
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; sql 4 spaces
(add-hook 'sql-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq c-basic-offset 4)))
;; Mac OS config
(when (string-equal system-type "darwin")
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:inherit nil :stipple nil :background "#282C34" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry nil :family "SF Mono")))))
    (setq mac-option-modifier nil)
    (setq mac-command-modifier 'meta)
    (setq ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "#2F4F4F")
          (run-with-idle-timer 0.1 nil
                               (lambda (bg) (set-face-background 'mode-line bg))
                               orig-bg)))))
