;;; Color theme
(require 'color-theme)
;; (add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/atom-one-dark-theme/"))
;; (add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/dracula-theme/"))
;; (require 'color-theme-sanityinc-tomorrow)
;; (load-theme 'sanityinc-tomorrow-eighties t)
;; (add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/emacs-material-theme"))
;; (load-theme 'material)
;;; https://github.com/hlissner/emacs-doom-themes
;; (add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/emacs-doom-themes/themes"))
(add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/tron-legacy-emacs-theme"))
(load-theme 'tron-legacy t)
;;; highlight current line number
(require 'hlinum)
(hlinum-activate)
;(global-linum-mode)
(set-face-foreground 'linum "dim gray")

(when (display-graphic-p)
  (setq initial-frame-alist '((width . 180) (height . 55))))
