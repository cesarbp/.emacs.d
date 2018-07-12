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
(require 'use-package)

(defun pnh-reinit-libs ()
  (interactive)
  (let ((generated-autoload-file (concat user-emacs-directory "my-autoload.el")))
    (dolist (d (directory-files (concat user-emacs-directory "lib") t "^[^\.]"))
      (dolist (f (directory-files d t "\\.el$"))
        (byte-compile-file f))
      (update-directory-autoloads d))))

(dolist (l (directory-files (concat user-emacs-directory "lib") nil "^[^\.]"))
  (add-to-list 'load-path (concat user-emacs-directory "lib/" l))
  (autoload (intern l) (concat l ".el")))

(when (not (file-exists-p (concat user-emacs-directory "my-autoload.el")))
  (pnh-reinit-libs))

(load (concat user-emacs-directory "my-autoload.el"))

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
;; Color theme
(require 'color-theme)
(add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/atom-one-dark-theme/"))
(load-theme 'atom-one-dark t)
;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)
