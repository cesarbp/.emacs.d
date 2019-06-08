;;; Erc
;; stolen from https://github.com/technomancy/dotfiles/blob/master/.emacs.d/phil/my-erc.el
(setq erc-prompt ">"
      erc-nick "cbp_"
      erc-server "chat.freenode.net"
      ;erc-track-priority-faces-only t
      erc-join-buffer 'bury
      erc-fill-column 75
      erc-header-line-format nil
      erc-track-exclude-types '("324" "329" "332" "333" "353" "477" "MODE"
                                "JOIN" "PART" "QUIT" "NICK")
      erc-lurker-threshold-time 3600
      
      erc-autojoin-timing :ident
      erc-flood-protect nil
      erc-server-send-ping-interval 45
      erc-server-send-ping-timeout 180
      erc-server-reconnect-timeout 60
      ;erc-server-flood-penalty 1000000
      erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#haskell"
                                     "#haskell-beginners" "#qfpl"))
      erc-prompt-for-nickserv-password nil
      erc-accidental-paste-threshold-seconds 0.5
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 14
      )
(eval-after-load 'erc
  '(progn
     (require 'erc-services)
     (require 'erc-truncate)
     (require 'notifications)
     (erc-services-mode 1)
     (erc-truncate-mode 1)
     (setq erc-complete-functions '(erc-pcomplete erc-button-next))
     (add-to-list 'erc-modules 'notifications)
     (define-key erc-mode-map (kbd "C-c r") 'pnh-reset-erc-track-mode)
     (define-key erc-mode-map (kbd "C-c C-M-SPC") 'erc-track-clear)
     (define-key erc-mode-map (kbd "C-u RET") 'browse-last-url-in-brower)))
(defun erc-track-clear ()
  (interactive)
  (setq erc-modified-channels-alist nil))

(defun browse-last-url-in-brower ()
  (interactive)
  (require 'ffap)
  (save-excursion
    (let ((ffap-url-regexp "\\(https?://\\)."))
      (ffap-next-url t t))))

(defun pnh-reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update)
  (erc-modified-channels-display))
