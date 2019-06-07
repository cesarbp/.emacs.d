;;; Color theme
(require 'color-theme)
;; (add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/atom-one-dark-theme/"))
;; (add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/dracula-theme/"))
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-eighties t)

;;; highlight current line number
(require 'hlinum)
(hlinum-activate)
(global-linum-mode)
(set-face-foreground 'linum "dim gray")
