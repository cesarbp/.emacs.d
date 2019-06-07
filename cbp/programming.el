;(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'idle-highlight-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook (lambda () (require 'multiple-cursors)))
(add-hook 'prog-mode-hook #'yas-minor-mode)


;;; Yasnippet
(eval-after-load 'yas-minor-mode
  '(yas-reload-all))

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
(eval-after-load 'elpy
  '(progn (elpy-enable)
          (setq python-shell-interpreter "ipython"
                python-shell-interpreter-args "-i --simple-prompt")
          (add-to-list 'python-shell-completion-native-disabled-interpreters
                       "jupyter")
          ;(require 'py-autopep8)
          (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)))
;; (setq python-shell-interpreter "python"
;;       python-shell-interpreter-args "-i"
;;       python-shell-prompt-detect-failure-warning nil)
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)

;;; javascript

(eval-after-load 'js2-mode
  '(progn (require 'js2-refactor)
          (require 'xref-js2)  ; requires the silversearcher (ag)
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
