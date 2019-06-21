;;; move-text.el --- Move current line or region with M-up or M-down.

;; filename: move-text.el
;; Description: Move current line or region with M-up or M-down.
;; Author: Jason Milkins <jasonm23@gmail.com>
;; Keywords: edit
;; Url: https://github.com/emacsfodder/move-text
;; Compatibility: GNU Emacs 25.1
;; Version: 2.0.8
;;
;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; MoveText 2.0.0 is a re-write of the old move-text and compatible with >= Emacs 25.1
;;
;; It allows you to move the current line using M-up / M-down if a
;; region is marked, it will move the region instead.
;;
;; Using the prefix (C-u *number* or META *number*) you can predefine how
;; many lines move-text will travel.
;;

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(provide 'move-text)

(global-set-key (kbd "C-x <up>") 'move-text-up)
(global-set-key (kbd "C-x <down>") 'move-text-down)
