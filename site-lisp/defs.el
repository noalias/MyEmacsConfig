;;; defs.el --- Variables,functions.             -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <noalias@LAPTOP-G0RSVTIK>
;; Keywords: tools

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
;;; VARS
(defconst win-p (eq system-type 'windows-nt))
(defconst win-wsl-p (and (eq system-type 'gnu/linux)
                         (string-match
                          "Linux.*Microsoft.*Linux"
                          (shell-command-to-string "uname -a"))))
(defconst linux-p (and (eq system-type 'gnu/linux)
                       (not (string-match
                             "Linux.*Microsoft.*Linux"
                             (shell-command-to-string "uname -a")))))

;;; FUNCTIONS
(defun find-file-externally (file)
  "Open files."
  (let ((file (expand-file-name file)))
    (cond
     ((and win-p (fboundp 'w32-shell-execute))
      (w32-shell-execute "cmd /c start" file))
     (win-wsl-p (call-process "wslview"
                              nil 0 nil
                              file))
     (linux-p (call-process "xdg-open"
                            nil 0 nil
                            file)))))

(provide 'defs)
;;; init-def.el ends here
