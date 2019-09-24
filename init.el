(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#696969" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#515151"])
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))

(let ((sf-mono (font-spec :name "SF Mono"))
      (source-code (font-spec :name "Source Code Pro"))
      (ubuntu (font-spec :name "Ubuntu Mono")))
  (cond ((find-font sf-mono)
         (set-face-attribute 'default nil :font sf-mono :height 92))
        ((find-font source-code)
         (set-face-attribute 'default nil :font source-code :height 120))
	((find-font ubuntu)
	 (set-face-attribute 'default nil :font ubuntu :height 108))))

(setq epa-armor t
      tls-checktrust 'ask
      el-get-allow-insecure nil
      gnutls-verify-error t
      network-security-level 'high)

;;;; ---- package management ----

;; be sure not to load stale bytecode-compiled lisp
(setq load-prefer-newer t)

;; this is where all subtree packages are
(defconst emacs-pkg-dir (concat user-emacs-directory "pkg"))
(defconst autoloads-file (concat emacs-pkg-dir "/my-autoload.el"))

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

;; from https://github.com/technomancy/emacs-starter-kit
(defun reinit-pkgs ()
  (interactive)
  (let ((generated-autoload-file autoloads-file))
    (dolist (d (directory-files emacs-pkg-dir t "^[^\.]"))
      (when (file-directory-p d)
        (dolist (f (directory-files d t "\\.el$"))
          (byte-compile-file f))
        (update-directory-autoloads d)))))

(when (not (file-exists-p autoloads-file))
  (reinit-pkgs)
  )

(add-to-list 'load-path emacs-pkg-dir)

(load autoloads-file)

(require 'better-defaults)

(mapc 'load (directory-files (concat user-emacs-directory "cbp")
                             t "^[^#].*el$"))

;;; --- Packages --- ;;;

;; exec-path-from-shell - make sure env looks the same inside emacs and in the user's shell in OSX
;(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; ido
;(require 'ido-completing-read+)
;(require 'flx-ido)
;;(ido-mode 1)        ; loaded by better-defaults
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
;; disable ido faces to see flx highlights.
;(setq ido-enable-flex-matching t)    ; loaded by better-defaults
(setq ido-use-faces nil)
;; icons - Note: do M-x all-the-icons-install-fonts
;(require 'memoize)
;(require 'all-the-icons)
;; Neotree
;(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;; magit
;; Magit has to be compiled manually for the autoloads to work
;; Follow the instructions at: https://magit.vc/manual/magit.html#Installing-from-the-Git-Repository
;; Create a file ~/.emacs.d/pkg/magit/config.mk with the contents:
;; LOAD_PATH  = -L ~/.emacs.d/pkg/magit/lisp
;; LOAD_PATH += -L ~/.emacs.d/pkg/dash.el
;; LOAD_PATH += -L ~/.emacs.d/pkg/hydra
;; LOAD_PATH += -L ~/.emacs.d/pkg/transient
;; LOAD_PATH += -L ~/.emacs.d/pkg/with-editor
;; LOAD_PATH += -L ~/.emacs.d/pkg/magit-popup
;; LOAD_PATH += -L ~/.emacs.d/pkg/ghub
;; And then run `make` inside magit's directory
(when (file-exists-p (concat emacs-pkg-dir "/magit/lisp/magit-autoloads.el"))
  (load (concat emacs-pkg-dir "/magit/lisp/magit-autoloads.el")))
;; Add hotkey for the autoloaded function
(define-key global-map "\C-xg" 'magit-status)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
	       (concat emacs-pkg-dir "/magit/Documentation/")))

;(require 'find-file-in-project)  ; https://github.com/technomancy/find-file-in-project
;; Undo Tree
;(require 'undo-tree)
(global-undo-tree-mode)
;; window number
(require 'window-number)
(window-number-mode)
(window-number-meta-mode)
;; projectile
(eval-after-load 'projectile
  '(progn
     (projectile-mode +1)
     (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))
;; dashboard
;(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents  . 15)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
;; org mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t
      org-remember-default-headline 'bottom
      org-completion-use-ido t)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
;; smart mode line
;(require 'smart-mode-line)
(sml/setup)
;; Prevent some minor modes from showing in the mode lines
(setq rm-blacklist
      (format "^ \\(%s\\)$"
              (mapconcat #'identity
                         '("Fly.*" "Projectile.*" "PgLn" "SliNav" "company"
                           "yas" "Undo-Tree" "ElDoc" "Paredit" "SP" "ARev"
                           "Hi")
                         "\\|")))
;; buffer-move
;(require 'buffer-move)
(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)

;;; custom
(setq inhibit-spash-screen t
      inhibit-startup-message t
      tab-always-indent 'complete
      shell-command-switch "-ic"
      gc-cons-threshold 20000000
      reb-re-syntax 'string  ; re-builder
      )

(winner-mode 1)
(put 'erase-buffer 'disabled nil)
(global-set-key (kbd "C-c o") 'occur)

