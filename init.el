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
(ido-ubiquitous-mode 1)
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
;; dashboard
(require 'page-break-lines)
(require 'dashboard)
(dashboard-setup-startup-hook)
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
 )

;; smart mode line
(require 'rich-minority)
(require 'smart-mode-line)
(sml/setup)
;; python
(require 'elpy)
(if (executable-find "flake8")
    (elpy-enable))
(require 'py-autopep8)
(if (and (executable-find "flake8") (executable-find "autopep8"))
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; custom
(setq inhibit-spash-screen t)
(setq inhibit-startup-message t)
(setq tab-always-indent 'complete)


