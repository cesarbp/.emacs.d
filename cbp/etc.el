(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(define-key global-map "\C-cs" 'select-current-line)

(global-set-key (kbd "<f5>") 'scroll-up-line)
(global-set-key (kbd "<f6>") 'scroll-down-line)
