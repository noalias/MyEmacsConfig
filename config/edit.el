;;; edit.el --- Config for easy edit.                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  LAPTOP-G0RSVTIK

;; Author: LAPTOP-G0RSVTIK <noalias@LAPTOP-G0RSVTIK>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(add-hook 'after-init-hook #'global-hl-line-mode)
(add-hook 'after-init-hook #'electric-pair-mode)

(add-hook 'after-init-hook #'show-paren-mode)
(setq show-paren-context-when-offscreen 'overlay
      show-paren-when-point-inside-paren t)

(add-hook 'after-init-hook #'auto-insert-mode)
(setq auto-insert-directory (no-littering-expand-etc-file-name "template"))

(setq-default fill-column 80
              tab-width 4
              indent-tabs-mode nil)

;;;; View mode
(setq view-read-only t)
(global-set-key (kbd "C-;") #'view-mode)
(with-eval-after-load 'view
    (define-keymap
      :keymap view-mode-map
      "j" #'next-line
      "k" #'previous-line
      "m" edit-mark-map
      "," #'View-back-to-mark))

;;;; Mark
(defvar-keymap edit-mark-map
  "." #'set-mark-command
  "s" #'mark-sexp
  "w" #'mark-word
  "b" #'mark-whole-buffer
  "f" #'mark-defun)
(global-set-key (kbd "C-x m") edit-mark-map)
;;;; Rectangle

(provide 'edit)
;;; edit.el ends here
