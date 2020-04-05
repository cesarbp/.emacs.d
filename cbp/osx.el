;;; Mac OS config
(when (string-equal system-type "darwin")
    (setq mac-option-modifier nil)
    (setq mac-command-modifier 'meta)
    (setq mac-right-option-modifier 'super)
    (setq ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "#2F4F4F")
          (run-with-idle-timer 0.1 nil
                               (lambda (bg) (set-face-background 'mode-line bg))
                               orig-bg)))))
