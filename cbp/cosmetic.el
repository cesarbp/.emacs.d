;;; Color theme
(require 'color-theme)
;; (add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/atom-one-dark-theme/"))
;; (add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/dracula-theme/"))
;; (require 'color-theme-sanityinc-tomorrow)
;; (load-theme 'sanityinc-tomorrow-eighties t)
;; (add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/emacs-material-theme"))
;; (load-theme 'material)
(add-to-list 'custom-theme-load-path (concat emacs-pkg-dir "/emacs-doom-themes/themes"))
(load-theme 'doom-outrun-electric)
;;; highlight current line number
(require 'hlinum)
(hlinum-activate)
;(global-linum-mode)
(set-face-foreground 'linum "dim gray")

;;; Mac OS config
(when (string-equal system-type "darwin")
    (setq mac-option-modifier nil)
    (setq mac-command-modifier 'meta)
    (setq ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "#2F4F4F")
          (run-with-idle-timer 0.1 nil
                               (lambda (bg) (set-face-background 'mode-line bg))
                               orig-bg)))))

(when (display-graphic-p)
  (setq initial-frame-alist '((width . 180) (height . 55))))
